-- some of the basic classes are uninheritable

class A inherits Int {};

class B inherits Bool {};

class C inherits String {};

class D inherits SELF_TYPE {};
