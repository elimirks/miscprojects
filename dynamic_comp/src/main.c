#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

/*
 * Who "owns" values?
 * - Scopes
 * - Curried calls
 * When should the refcount increase?
 * - When a new scope is created
 * - When a curry references a value
 * When should a refcount decrease?
 * - When a scope is destroyed
 * - When a curried call is destroyed
 *
 *
 * What about closures?!?
 * Perhaps, they can have their own scopes
 * Actually, that's the same as root level functions, except they must refer to
 * the root scope. There really isn't any difference!
 *
 * What are parameter lists?
 * Are they considered scopes too? I don't think so.
 * After all, function parameter lists are ephemeral - only created when
 * calling a function, and destroyed afterwards
 * Generated functions should initially create a new scope, and push all the
 * parameters into that scope.
 * This _could_ be optimized in the future. But that's the simplest strategy.
 *
 * What about curried calls?
 * Curried calls are an exception. They must also inc/dec refcounts.
 *
 *
 * Who "owns" scopes?
 * - Running function code (including `main()`)
 * - Function declarations own the scope that they're defined in
 */

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
    struct Value **parameters;
} Call;

// TODO: Maybe don't allocate for ints or other primitives..
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

Value * refValue(Value *value) {
    value->refCount += 1;
    return value;
}

void derefValue(Value *value) {
    assert(value->refCount > 0);
    if (value->refCount == 1) {
        free(value);
    } else {
        value->refCount -= 1;
    }
}

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

typedef struct ValueList {
    uint32_t len;
    Value **data;
} ValueList;

ValueList newValueList() {
    ValueList new = {
        .len = 0,
        .data = NULL,
    };
    return new;
}

void addToValueList(ValueList *list, Value *value) {
    list->len += 1;
    list->data = realloc(list->data, list->len * sizeof(Value *));
    list->data[list->len - 1] = refValue(value);
}

void derefValueList(ValueList list) {
    if (list.len > 0) {
        for (uint32_t i = 0; i < list.len; i++) {
            derefValue(list.data[i]);
        }
        free(list.data);
    }
}

void refValueList(ValueList *list) {
    for (uint32_t i = 0; i < list->len; i++) {
        refValue(list->data[i]);
    }
}

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

Value * newIntValue(int64_t num) {
    Value *new = malloc(sizeof(Value));
    new->type = ValueTypeInt;
    //new->refCount = 1; This would mean it references itself, right?
    new->refCount = 0;
    new->intValue= num;
    return new;
}

Value * Builtin_add(Value *first, Value *second) {
    assert(first->type == second->type);

    Value *result = malloc(sizeof(Value));
    result->type = first->type;
    result->refCount = 1;

    switch (first->type) {
    case ValueTypeInt:
        result->intValue = first->intValue + second->intValue;
        break;
    case ValueTypeFloat:
        result->floatValue = first->floatValue + second->floatValue;
        break;
    default:
        // TODO: Proper stack trace
        panic("Not implemented");
    }
    derefValue(first);
    derefValue(second);
    return result;
}

Value * Builtin_sub(Value *first, Value *second) {
    assert(first->type == second->type);

    Value *result = malloc(sizeof(Value));
    result->type = first->type;
    result->refCount = 1;

    switch (first->type) {
    case ValueTypeInt:
        result->intValue = first->intValue - second->intValue;
        break;
    case ValueTypeFloat:
        result->floatValue = first->floatValue - second->floatValue;
        break;
    default:
        // TODO: Proper stack trace
        panic("Not implemented");
    }
    derefValue(first);
    derefValue(second);
    return result;
}

Value * Builtin_mul(Value *first, Value *second) {
    assert(first->type == second->type);

    Value *result = malloc(sizeof(Value));
    result->type = first->type;
    result->refCount = 1;

    switch (first->type) {
    case ValueTypeInt:
        result->intValue = first->intValue * second->intValue;
        break;
    case ValueTypeFloat:
        result->floatValue = first->floatValue * second->floatValue;
        break;
    default:
        // TODO: Proper stack trace
        panic("Not implemented");
    }
    derefValue(first);
    derefValue(second);
    return result;
}

// Example of what a complied function could look like
Value * User_fact(ValueList values) {
    assert(values.len == 1);
    assert(values.data[0]->type == ValueTypeInt);

    if (values.data[0]->intValue <= 1) {
        Value *result = refValue(values.data[0]);
        derefValueList(values);
        return result;
    } else {
        Value *decremented = refValue(Builtin_sub(refValue(values.data[0]), refValue(newIntValue(1))));
        ValueList newParams = newValueList();
        // FIXME: Decide on where ref counts should be incremented or decremented
        // Who's responsible for freeing data?! Is it the call site, or at the function return??
        addToValueList(&newParams, decremented);
        refValueList(&newParams);
        Value *next = User_fact(newParams);
        Value *result = Builtin_mul(next, refValue(values.data[0]));
        derefValueList(newParams);
        derefValueList(values);
        return result;
    }
}

static void (*callTable[])(Value *) = {
};

int main() {
    Scope *rootScope = newScope(NULL);
    Value *result;

    result = Builtin_add(refValue(newIntValue(5)), refValue(newIntValue(2)));
    printf("%lld\n", result->intValue);

    ValueList params = newValueList();
    addToValueList(&params, newIntValue(4));
    result = User_fact(params);
    printf("%lld\n", result->intValue);
    return 0;
}
