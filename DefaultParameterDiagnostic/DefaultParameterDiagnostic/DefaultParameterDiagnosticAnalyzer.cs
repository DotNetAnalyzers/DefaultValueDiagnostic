// Copyright (c) Sam Harwell. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE in the project root for license information.

namespace DefaultParameterDiagnostic
{
    using System.Collections.Immutable;
    using System.Linq;
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    using Microsoft.CodeAnalysis.Diagnostics;

    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class DefaultParameterDiagnosticAnalyzer : DiagnosticAnalyzer
    {
        public const string MissingDefaultValueDiagnosticId = "MissingDefaultValue";
        internal const string MissingDefaultValueTitle = "Override does not include a default value which is specified in the overridden method's signature.";
        internal const string MissingDefaultValueMessageFormat = "Parameter '{0}' does not specify a default value, but method '{1}' does specify a default value for the parameter";
        internal const string MissingDefaultValueCategory = "API";
        internal const string MissingDefaultValueDescription = "Override does not include a default value which is specified in the overridden method's signature.";

        internal static readonly DiagnosticDescriptor MissingDefaultValueDiagnostic =
            new DiagnosticDescriptor(MissingDefaultValueDiagnosticId, MissingDefaultValueTitle, MissingDefaultValueMessageFormat, MissingDefaultValueCategory, DiagnosticSeverity.Warning, true, MissingDefaultValueDescription);

        public const string DefaultValueMismatchDiagnosticId = "DefaultValueMismatch";
        internal const string DefaultValueMismatchTitle = "Override specifies a different default value than appears in the overridden method's signature.";
        internal const string DefaultValueMismatchMessageFormat = "Parameter '{0}' does not specify the same default value as '{1}'";
        internal const string DefaultValueMismatchCategory = "API";
        internal const string DefaultValueMismatchDescription = "Override specifies a different default value than appears in the overridden method's signature.";

        internal static readonly DiagnosticDescriptor DefaultValueMismatchDiagnostic =
            new DiagnosticDescriptor(DefaultValueMismatchDiagnosticId, DefaultValueMismatchTitle, DefaultValueMismatchMessageFormat, DefaultValueMismatchCategory, DiagnosticSeverity.Warning, true, DefaultValueMismatchDescription);

        private static readonly ImmutableArray<DiagnosticDescriptor> _supportedDiagnostics =
            ImmutableArray.Create(MissingDefaultValueDiagnostic, DefaultValueMismatchDiagnostic);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
        {
            get
            {
                return _supportedDiagnostics;
            }
        }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeBaseListSyntax, SyntaxKind.BaseList);
            context.RegisterSyntaxNodeAction(AnalyzeMethodDeclarationSyntax, SyntaxKind.MethodDeclaration);
        }

        private void AnalyzeBaseListSyntax(SyntaxNodeAnalysisContext context)
        {
            BaseListSyntax syntax = context.Node as BaseListSyntax;
            if (syntax == null)
                return;

            INamedTypeSymbol declaringType = context.SemanticModel.GetDeclaredSymbol(syntax.Parent, context.CancellationToken) as INamedTypeSymbol;
            if (declaringType == null)
                return;

            // check interfaces declared on this type
            foreach (var interfaceType in declaringType.Interfaces)
            {
                foreach (var method in interfaceType.GetMembers().OfType<IMethodSymbol>())
                {
                    if (!HasDefaultParameters(method))
                        continue;

                    var implementingMethod = declaringType.FindImplementationForInterfaceMember(method) as IMethodSymbol;
                    if (implementingMethod == null)
                        continue;

                    CheckDefaultValues(context, implementingMethod, method);
                }
            }
        }

        private void AnalyzeMethodDeclarationSyntax(SyntaxNodeAnalysisContext context)
        {
            MethodDeclarationSyntax syntax = context.Node as MethodDeclarationSyntax;
            if (syntax == null)
                return;

            // Interface members are handled by AnalyzeBaseListSyntax. All other cases require the `override` keyword to
            // appear.
            if (!syntax.Modifiers.Any(i => i.CSharpKind() == SyntaxKind.OverrideKeyword))
                return;

            // No work to do if there are no parameters
            if (!(syntax.ParameterList?.Parameters.Count > 0))
                return;

            // If the first parameter has a default value, all parameters have a default value and the
            // MissingDefaultValueDiagnostic does not apply.
            if (syntax.ParameterList.Parameters.First().Default != null)
                return;

            SemanticModel semanticModel = context.SemanticModel;
            IMethodSymbol methodSymbol = semanticModel.GetDeclaredSymbol(syntax, context.CancellationToken) as IMethodSymbol;
            if (methodSymbol == null)
                return;

            IMethodSymbol overriddenMethod = methodSymbol.OverriddenMethod;
            if (overriddenMethod == null)
                return;

            CheckDefaultValues(context, methodSymbol, overriddenMethod);
        }

        private void CheckDefaultValues(SyntaxNodeAnalysisContext context, IMethodSymbol method, IMethodSymbol baseMethod)
        {
            var methodParameters = method.Parameters;
            var baseMethodParameters = baseMethod.Parameters;
            if (method.Parameters.Length != baseMethod.Parameters.Length)
                return;

            for (int i = methodParameters.Length - 1; i >= 0; i--)
            {
                // assume valid signature, where all parameters with default values appear at the end of the argument
                // list
                if (!baseMethodParameters[i].HasExplicitDefaultValue)
                    return;

                if (methodParameters[i].HasExplicitDefaultValue)
                    CheckForDefaultValueMismatch(context, method, methodParameters[i], baseMethod, baseMethodParameters[i]);
                else
                    ReportMissingDefaultValue(context, method, methodParameters[i], baseMethod, baseMethodParameters[i]);
            }
        }

        private void ReportMissingDefaultValue(SyntaxNodeAnalysisContext context, IMethodSymbol method, IParameterSymbol parameter, IMethodSymbol baseMethod, IParameterSymbol baseParameter)
        {
            var location = parameter.Locations.FirstOrDefault();
            if (!location.IsInSource)
                return;

            int position = location.SourceSpan.Start;
            string methodName = GetQualifiedMethodName(context.SemanticModel, position, method);
            string baseMethodName = GetQualifiedMethodName(context.SemanticModel, position, baseMethod);
            context.ReportDiagnostic(Diagnostic.Create(MissingDefaultValueDiagnostic, location, methodName, baseMethodName));
        }

        private void CheckForDefaultValueMismatch(SyntaxNodeAnalysisContext context, IMethodSymbol method, IParameterSymbol parameter, IMethodSymbol baseMethod, IParameterSymbol baseParameter)
        {
            if (object.Equals(parameter.ExplicitDefaultValue, baseParameter.ExplicitDefaultValue))
                return;

            var location = parameter.Locations.FirstOrDefault();
            if (!location.IsInSource)
                return;

            int position = location.SourceSpan.Start;
            string methodName = GetQualifiedMethodName(context.SemanticModel, position, method);
            string baseMethodName = GetQualifiedMethodName(context.SemanticModel, position, baseMethod);
            context.ReportDiagnostic(Diagnostic.Create(DefaultValueMismatchDiagnostic, location, methodName, baseMethodName));
        }

        private static string GetQualifiedMethodName(SemanticModel semanticModel, int position, IMethodSymbol method)
        {
            string typeName = method.ContainingType.ToMinimalDisplayString(semanticModel, position, SymbolDisplayFormat.MinimallyQualifiedFormat);
            return "\{typeName}.\{method.Name}";
        }

        private static bool HasDefaultParameters(IMethodSymbol method)
        {
            var lastParameter = method.Parameters.LastOrDefault();
            if (lastParameter == null)
                return false;

            return lastParameter.HasExplicitDefaultValue;
        }
    }
}
