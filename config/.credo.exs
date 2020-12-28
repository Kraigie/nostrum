%{
  configs: [
    %{
      name: "default",
      files: %{
        included: ["lib/", "src/", "web/", "apps/"],
        excluded: [~r"/_build/", ~r"/deps/"]
      },
      requires: [],
      check_for_updates: true,
      checks: [
        {Credo.Check.Consistency.ExceptionNames},
        {Credo.Check.Consistency.LineEndings, false},
        {Credo.Check.Consistency.SpaceAroundOperators},
        {Credo.Check.Consistency.SpaceInParentheses},
        {Credo.Check.Consistency.TabsOrSpaces},
        {Credo.Check.Design.AliasUsage, priority: :low},
        {Credo.Check.Design.DuplicatedCode, excluded_macros: [], mass_threshold: 50},
        {Credo.Check.Design.TagTODO, exit_status: 0},
        {Credo.Check.Design.TagFIXME},
        {Credo.Check.Readability.FunctionNames},
        {Credo.Check.Readability.LargeNumbers},
        {Credo.Check.Readability.MaxLineLength, priority: :low, max_length: 200},
        {Credo.Check.Readability.ModuleAttributeNames},
        {Credo.Check.Readability.ModuleDoc},
        {Credo.Check.Readability.ModuleNames},
        {Credo.Check.Readability.ParenthesesInCondition},
        {Credo.Check.Readability.PredicateFunctionNames},
        {Credo.Check.Readability.TrailingBlankLine, false},
        {Credo.Check.Readability.TrailingWhiteSpace, false},
        {Credo.Check.Readability.VariableNames},
        {Credo.Check.Refactor.ABCSize, false},
        {Credo.Check.Refactor.CondStatements},
        {Credo.Check.Refactor.FunctionArity},
        {Credo.Check.Refactor.MatchInCondition},
        {Credo.Check.Refactor.PipeChainStart, false},
        {Credo.Check.Refactor.CyclomaticComplexity, false},
        {Credo.Check.Refactor.NegatedConditionsInUnless},
        {Credo.Check.Refactor.NegatedConditionsWithElse},
        {Credo.Check.Refactor.Nesting},
        {Credo.Check.Refactor.UnlessWithElse},
        {Credo.Check.Warning.IExPry},
        {Credo.Check.Warning.IoInspect},
        # {Credo.Check.Warning.NameRedeclarationByAssignment},
        # {Credo.Check.Warning.NameRedeclarationByCase},
        # {Credo.Check.Warning.NameRedeclarationByDef},
        # {Credo.Check.Warning.NameRedeclarationByFn},
        {Credo.Check.Warning.OperationOnSameValues},
        {Credo.Check.Warning.BoolOperationOnSameValues},
        {Credo.Check.Warning.UnusedEnumOperation},
        {Credo.Check.Warning.UnusedKeywordOperation},
        {Credo.Check.Warning.UnusedListOperation},
        {Credo.Check.Warning.UnusedStringOperation},
        {Credo.Check.Warning.UnusedTupleOperation},
        {Credo.Check.Warning.OperationWithConstantResult}
      ]
    }
  ]
}
