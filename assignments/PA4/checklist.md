> The following check list is adapted from [afterthat97/cool-compiler](https://github.com/afterthat97/cool-compiler/tree/master/assignments/PA4). Thank you afterthat97!

# Class

- [ ] Add 5 basic classes (`Object`, `IO`, `Int`, `Bool`, `Str`) to class table
- [ ] Check if class `Main` and method `main` is defined (expected)
- [ ] Check if `SELF_TYPE` is defined (not expected)
- [ ] Check class or method redefinition (not expected)
- [ ] Check if any class inherits from `Int`, `Str`, `Bool`, `SELF_TYPE`, or an undefined class (not expected)
- [ ] Check if its parent class (`Object` by default) exists (expected)
- [ ] Check inheritance cycle (not expected)

# Method

- [ ] Check if the number of arguments, the types of the formal parameters, and the return type are exactly the same when a class tries to override the inherited method from an ancestor class
- [ ] Check if `self` is the name of some formal parameter (not expected)
- [ ] Check formal parameter redefinition (not expected)
- [ ] Check if the declared type of formal parameter is defined (expected)
- [ ] Check if the return type is defined (expected)
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
