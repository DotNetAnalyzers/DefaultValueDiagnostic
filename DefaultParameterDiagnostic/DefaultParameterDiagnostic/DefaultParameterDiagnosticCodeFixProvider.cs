// Copyright (c) Sam Harwell. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE in the project root for license information.

namespace DefaultParameterDiagnostic
{
    using System;
    using System.Collections.Immutable;
    using System.Composition;
    using System.Linq;
    using System.Threading;
    using System.Threading.Tasks;
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CodeActions;
    using Microsoft.CodeAnalysis.CodeFixes;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    using Microsoft.CodeAnalysis.Formatting;
    using Microsoft.CodeAnalysis.Simplification;

    [ExportCodeFixProvider("DefaultParameterDiagnosticCodeFixProvider", LanguageNames.CSharp)]
    [Shared]
    public class DefaultParameterDiagnosticCodeFixProvider : CodeFixProvider
    {
        private static readonly ImmutableArray<string> _fixableDiagnostics =
            ImmutableArray.Create(DefaultParameterDiagnosticAnalyzer.MissingDefaultValueDiagnosticId);

        public sealed override ImmutableArray<string> GetFixableDiagnosticIds()
        {
            return _fixableDiagnostics;
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override Task ComputeFixesAsync(CodeFixContext context)
        {
            foreach (var diagnostic in context.Diagnostics)
            {
                if (diagnostic.Id.Equals(DefaultParameterDiagnosticAnalyzer.MissingDefaultValueDiagnosticId, StringComparison.Ordinal))
                    context.RegisterFix(CodeAction.Create("Add default value", cancellationToken => AddDefaultValueAsync(context.Document, diagnostic, cancellationToken)), diagnostic);
            }

            return Task.FromResult(default(object));
        }

        private async Task<Document> AddDefaultValueAsync(Document document, Diagnostic diagnostic, CancellationToken cancellationToken)
        {
            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            var diagnosticSpan = diagnostic.Location.SourceSpan;

            // Find the parameter identified by the diagnostic.
            var parameterSyntax = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ParameterSyntax>().FirstOrDefault();
            if (parameterSyntax == null)
                return document;

            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            if (semanticModel == null)
                return document;

            IParameterSymbol parameterSymbol = semanticModel.GetDeclaredSymbol(parameterSyntax, cancellationToken);
            if (parameterSymbol == null)
                return document;

            IMethodSymbol containingMethod = parameterSymbol.ContainingSymbol as IMethodSymbol;
            if (containingMethod == null)
                return document;

            // might be from an overridden method (easiest)
            Document result = await TryApplyDefaultValue(document, root, parameterSyntax, parameterSymbol, containingMethod, containingMethod.OverriddenMethod, cancellationToken).ConfigureAwait(false);
            if (result != null)
                return result;

            // might be from an explicitly implemented interface method (easy)
            foreach (var baseMethod in containingMethod.ExplicitInterfaceImplementations)
            {
                result = await TryApplyDefaultValue(document, root, parameterSyntax, parameterSymbol, containingMethod, baseMethod, cancellationToken).ConfigureAwait(false);
                if (result != null)
                    return result;
            }

            // might be from an implicitly implemented interface
            foreach (var interfaceType in containingMethod.ContainingType.Interfaces)
            {
                foreach (var member in interfaceType.GetMembers(containingMethod.Name).OfType<IMethodSymbol>())
                {
                    IMethodSymbol implementingMethod = containingMethod.ContainingType.FindImplementationForInterfaceMember(member) as IMethodSymbol;
                    if (implementingMethod != containingMethod)
                        continue;

                    result = await TryApplyDefaultValue(document, root, parameterSyntax, parameterSymbol, containingMethod, member, cancellationToken).ConfigureAwait(false);
                    if (result != null)
                        return result;
                }
            }

            // no idea?
            return document;
        }

        private async Task<Document> TryApplyDefaultValue(Document document, SyntaxNode root, ParameterSyntax parameterSyntax, IParameterSymbol parameter, IMethodSymbol method, IMethodSymbol baseMethod, CancellationToken cancellationToken)
        {
            if (baseMethod == null)
                return null;

            IParameterSymbol baseParameter = baseMethod.Parameters[parameter.Ordinal];
            if (!baseParameter.HasExplicitDefaultValue)
                return null;

            return await TryApplyDefaultValue(document, root, parameterSyntax, parameter, baseParameter, cancellationToken).ConfigureAwait(false);
        }

        private async Task<Document> TryApplyDefaultValue(Document document, SyntaxNode root, ParameterSyntax parameterSyntax, IParameterSymbol parameter, IParameterSymbol baseParameter, CancellationToken cancellationToken)
        {
            ExpressionSyntax defaultValueSyntax = null;

            foreach (var syntaxReference in baseParameter.DeclaringSyntaxReferences)
            {
                SyntaxNode syntax = await syntaxReference.GetSyntaxAsync(cancellationToken).ConfigureAwait(false);
                if (syntax == null)
                    continue;

                ParameterSyntax baseParameterSyntax = syntax as ParameterSyntax;
                defaultValueSyntax = baseParameterSyntax?.Default?.Value;
                if (defaultValueSyntax != null)
                    break;
            }

            if (defaultValueSyntax == null)
            {
                defaultValueSyntax = SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Identifier("UnknownConstantValue"));
                defaultValueSyntax = SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, defaultValueSyntax);
                defaultValueSyntax = SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, defaultValueSyntax);
            }

            ParameterSyntax newParameterSyntax = parameterSyntax
                .WithDefault(SyntaxFactory.EqualsValueClause(defaultValueSyntax))
                .WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation);
            SyntaxNode newRoot = root.ReplaceNode(parameterSyntax, newParameterSyntax);
            return document.WithSyntaxRoot(newRoot);
        }
    }
}
