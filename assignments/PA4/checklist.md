> The following check list is adapted from [afterthat97/cool-compiler](https://github.com/afterthat97/cool-compiler/tree/master/assignments/PA4). Thank you afterthat97!

# Class

- [x] Add 5 basic classes (`Object`, `IO`, `Int`, `Bool`, `Str`) to class table: indirectly with [redefined_basic.cl](tests/class/redefined_basic.cl)
- [x] Check if `SELF_TYPE` is defined (not expected): [redefined_SELF_TYPE.cl](tests/class/redefined_SELF_TYPE.cl)
- [x] Check class redefinition (not expected): [redefined_basic.cl](tests/redefined_basic.cl), [redefined_class.cl](tests/class/redefined_class.cl)
- [x] Check if any class inherits from `Int`, `Str`, `Bool`, `SELF_TYPE` (not expected): [from_basic.cl](tests/class/from_basic.cl)
- [x] Check if its parent class (`Object` by default) exists (expected): [undeclared_base.cl](tests/class/undeclared_base.cl)
- [x] Check inheritance cycle (not expected): [circular.cl](tests/class/circular.cl)

# Method

- [x] Check method redefinition in single class (not expected): [redefined_method.cl](tests/method/redefined_method.cl)
- [x] Check if the number of arguments, the types of the formal parameters, and the return type are exactly the same when a class tries to override the inherited method from an ancestor class: [unmatched_override](tests/method/unmatched_override.cl)
- [x] Check if class `Main` and method `main` is defined (expected): [no_Main.cl](tests/method/no_Main.cl), [no_main_method.cl](tests/method/no_main_method.cl)
- [x] Check if `self` is the name of some formal parameter (not expected): [self_as_formal.cl](tests/method/self_as_formal.cl)
- [x] Check formal parameter redefinition (not expected): [formals_with_same_name.cl](tests/method/formals_with_same_name.cl)
- [x] Check if the declared type of formal parameter is defined (expected): [undefined_formal_type.cl](tests/method/undefined_formal_type.cl)
- [x] Check if the declared type of formal parameter is SELF_TYPE (not expected): [no_formal_type_SELF_TYPE.cl](tests/no_formal_type_SELF_TYPE.cl)
- [x] Check if the return type is defined or is SELF_TYPE (expected): [undefined_return_type.cl](tests/method/undefined_return_type.cl)
- [x] Check if the inferred return type conforms to the declared return type (expected): [not_conform_return_type.cl](tests/not_conform_return_type.cl)

# Attribute

- [x] Check attribute redefinition (not expected): [redefined_attr.cl](tests/method/redefined_attr.cl)
- [x] Check if the declared type of attribute is defined or is SELF_TYPE (expected): [undefined_attr_type.cl](tests/method/undefined_attr_type.cl), [conform_SELF_TYPE.cl](tests/conform_SELF_TYPE.cl)
- [x] Check if the inferred type of initialization of attribute (if exists) conforms to its declared type (expected): [attr.cl](tests/attr.cl)

# Assign

- [x] Check if the identifier is declared (expected): [undefined_assign_identifier.cl](tests/undefined_assign_identifier.cl)
- [x] Check if the type of assigned expression conforms to declared type of identifier (expected): [not_conform_assign.cl](tests/not_conform_assign.cl)

# Dispatch

- [ ] Check if the declared static dispatch type is defined (only for static dispatch, expected)
- [ ] Check if the expression type is defined (expected)
- [ ] Check if the expression type conforms to the declared static dispatch type (only for static dispatch, expected)
- [ ] Check if the method is defined (expected)
- [ ] Check if the types of actual parameters conform to the declared type of formal parameters in call of method (expected)
- [ ] Check if the number of arguments match in call of method (expected)

# Condition & Loop

- [x] Check if the condition has type `Bool` (expected): [if.cl](tests/if.cl), [while.cl](tests/while.cl)

# Case / Branch

- [x] Check if duplicate branch exists (not expected): [case.cl](tests/case.cl)

# Let

- [x] Check if the declared type of identifier is defined or is SELF_TYPE (expected): [undefined_let_id_type.cl](tests/undefined_let_id_type.cl), [let_id_with_SELF_TYPE.cl](tests/let_id_with_SELF_TYPE.cl)
- [x] Check if the inferred type of initialization of identifier (if exists) conforms to its declared type (expected): [not_conform_let_id_type.cl](tests/not_conform_let_id_type.cl)

# Plus, Sub, Mul, Div (`+`, `-`, `*`, `/`)

- [x] Check if all arguments have type `Int` (expected): [arith.cl](tests/arith.cl)

# Neg, Lt, Leq (`~`, `<`, `<=`)

- [x] Check if all arguments have type `Int` (expected): [neg.cl](tests/neg.cl), [comparison.cl](tests/comparison.cl)

# EQ (`=`)

- [x] Check if one argument has static type `Int`,  `Bool`, or `String`, then the other must have the same static type: [eq.cl](tests/eq.cl)

# Comp (not)

- [x] Check if argument has type `Bool` (expected): [comp.cl](tests/comp.cl)

# New

- [x] Check if `new` is used with defined class or SELF_TYPE (expected): [new.cl](tests/new.cl), [new_SELF_TYPE.cl](tests/new_SELF_TYPE.cl)

# Object

- [x] Check if identifier is declared (expected): [undefined_object.cl](tests/undefined_object.cl)
