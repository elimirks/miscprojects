#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

void panic(const char *message) {
    fprintf(stderr, "%s", message);
    exit(1);
}

typedef enum ValueType {
    ValueTypeInt = 0,
    ValueTypeFloat,
    ValueTypeCall,
} ValueType;

// Represents a curried constructor or function invocation
typedef struct Call {
    //uint32_t callId;
    // Null terminates list of parameters
    struct Value *parameters;
} Call;

typedef struct Value {
    // For garbage collection
    uint32_t refCount;
    ValueType type;
    union {
        double floatValue; 
        int64_t intValue; 
        Call callValue;
    };
} Value;

typedef struct Scope {
    uint32_t refCount;
    uint32_t symbolCount;
    // Might be better to use a HashMap in the future
    // Also, it would be good to have a symbolId -> symbolName map instead
    // So that we don't have to deal with so many string allocations!
    char **symbolNames;
    Value *symbolValues;
    struct Scope *parent;
} Scope;

Scope * newScope(Scope *parent) {
    if (parent) {
        parent->refCount += 1;
    }
    Scope *new = malloc(sizeof(Scope));
    new->refCount = 1;
    new->symbolCount = 0;
    new->parent = parent;
    return new;
}

Value newIntValue(int64_t num) {
    Value new = {
        .type = ValueTypeInt,
        .refCount = 1,
        .intValue= num,
    };
    return new;
}

Value add(Value first, Value second) {
    assert(first.type == second.type);
    first.refCount -= 1;
    second.refCount -= 1;

    Value result;
    result.type = first.type;
    result.refCount = 1;

    switch (first.type) {
    case ValueTypeInt:
        result.intValue = first.intValue + second.intValue;
        break;
    case ValueTypeFloat:
        result.floatValue = first.floatValue + second.floatValue;
        break;
    case ValueTypeCall:
        panic("Not implemented");
    }
    return result;
}

int main() {
    Scope *rootScope = newScope(NULL);
    Value result = add(newIntValue(5), newIntValue(2));
    printf("%ld\n", result.intValue);
    return 0;
}
