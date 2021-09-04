#pragma once

#include <map>

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

extern void HandleDefinition();
extern void HandleExtern();
extern void HandleTopLevelExpression();
