namespace DefaultParameterDiagnostic
{
    using System.Collections.Immutable;
    using System.Composition;
    using System.Threading.Tasks;
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CodeFixes;

    [ExportCodeFixProvider("DefaultParameterDiagnosticCodeFixProvider", LanguageNames.CSharp), Shared]
    public class DefaultParameterDiagnosticCodeFixProvider : CodeFixProvider
    {
        private static readonly ImmutableArray<string> _fixableDiagnostics =
            ImmutableArray.Create<string>();

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
            return Task.FromResult(default(object));
        }
    }
}