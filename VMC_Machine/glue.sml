structure WHILEEXPLrVals = WHILEEXPLrValsFun(structure Token = LrParser.Token);

structure WHILELex = WHILELexFun(structure Tokens = WHILEEXPLrVals.Tokens);

structure WHILEParser = JoinWithArg(
                                structure ParserData = WHILEEXPLrVals.ParserData
                                structure Lex = WHILELex
                                structure LrParser = LrParser
                                );
