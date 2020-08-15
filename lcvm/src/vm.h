#ifndef __VM_H__
#define __VM_H__

#include "scanner.h"

void printExpression(Expression *expr);
Expression *evaluateExpression(Expression *expr);

#endif
