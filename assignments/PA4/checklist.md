> The following check list is adapted from [afterthat97/cool-compiler](https://github.com/afterthat97/cool-compiler/tree/master/assignments/PA4). Thank you afterthat97!

# Class

- [x] Add 5 basic classes (`Object`, `IO`, `Int`, `Bool`, `Str`) to class table: indirectly with [redefined_basic.cl](tests/class/redefined_basic.cl)
- [x] Check if class `Main` and method `main` is defined (expected): [no_Main.cl](tests/class/no_Main.cl), [no_main_method.cl](tests/class/no_main_method.cl)
- [x] Check if `SELF_TYPE` is defined (not expected): [redefined_SELF_TYPE.cl](tests/class/redefined_SELF_TYPE.cl)
- [x] Check class or method redefinition (not expected): [redefined_basic.cl](tests/redefined_basic.cl), [redefined_class.cl](tests/class/redefined_class.cl)
- [x] Check if any class inherits from `Int`, `Str`, `Bool`, `SELF_TYPE` (not expected): [from_basic.cl](tests/class/from_basic.cl)
- [x] Check if its parent class (`Object` by default) exists (expected): [undeclared_base.cl](tests/class/undeclared_base.cl)
- [x] Check inheritance cycle (not expected): [circular.cl](tests/class/circular.cl)

# Method

- [ ] Check if the number of arguments, the types of the formal parameters, and the return type are exactly the same when a class tries to override the inherited method from an ancestor class: [unmatched_override](tests/method/unmatched_override.cl)
- [ ] Check if `self` is the name of some formal parameter (not expected): [self_as_formal.cl](tests/method/self_as_formal.cl)
- [ ] Check formal parameter redefinition (not expected): [formals_with_same_name.cl](tests/method/formals_with_same_name.cl)
- [ ] Check if the declared type of formal parameter is defined (expected): [undefined_formal_type.cl](tests/method/undefined_formal_type.cl)
- [ ] Check if the return type is defined (expected): [undefined_return_type.cl](tests/method/undefined_return_type.cl)
- [ ] Check if the inferred return type conforms to the declared return type (expected)

# Attribute

- [ ] Check if the declared type of attribute is defined (expected)
- [ ] Check if the inferred type of initialization of attribute (if exists) conforms to its declared type (expected)
Assign
- [ ] Check if the identifier is declared (expected)
- [ ] Check if the type of assigned expression conforms to declared type of identifier (expected)
Dispatch
- [ ] Check if the declared static dispatch type is defined (only for static dispatch, expected)
- [ ] Check if the expression type is defined (expected)
- [ ] Check if the expression type conforms to the declared static dispatch type (only for static dispatch, expected)
- [ ] Check if the method is defined (expected)
- [ ] Check if the types of actual parameters conform to the declared type of formal parameters in call of method (expected)
- [ ] Check if the number of arguments match in call of method (expected)

# Condition & Loop

- [ ] Check if the condition has type `Bool` (expected)

# Case / Branch

- [ ] Check if duplicate branch exists (not expected)
Let
- [ ] Check if the declared type of identifier is defined (expected)
- [ ] Check if the inferred type of initialization of identifier (if exists) conforms to its declared type (expected)

# Plus, Sub, Mul, Div, Neg, LT, LEQ (`+`, `-`, `*`, `/`, `~`, `<`, `<=`)

- [ ] Check if all arguments have type `Int` (expected)

# EQ (`=`)

- [ ] Check if one argument has static type `Int`,  `Bool`, or `String`, then the other must have the same static type

# Comp (not)

- [ ] Check if argument has type `Bool` (expected)

# New

- [ ] Check if `new` is used with undefined class (not expected)

# Object

- [ ] Check if identifier is declared (expected)
