# Syntax of subset of C processed by compiler

A program consists of a single function definition.

A function definition looks like this:

```
int functionName (int varName, int varName, ...) {
  functionBody
}
```

or

```
void functionName (int varName, int varName, ...) {
  functionBody
}
```

It has a return type (which must be either int or void), a name (any name of
your choosing), and a list of zero or more parameters, separated by commas and
enclosed in parentheses. Each parameter has a type (which must be int) followed
by a parameter name (of your choosing). The function body is enclosed with
braces.

A function body is a list of zero or more local variable declarations followed
by a list of zero or more statements.

A local variable declaration looks like this:

```
int varName = constant;
```

It has a type (which must be int), a variable name (of your choosing), an equals
sign, an integer constant, and a semicolon.

There are five kinds of statements:

* An assignment statement looks like this:

    ```
    varName = exp;
    ```

    It has a variable name (which must have been previously declared as a
parameter or local variable), an equals sign, an integer-valued expression, and
a semicolon.

* A return statement looks like this:

    ```
    return exp;
    ```

    It has the keyword 'return', an integer-valued expression, and a
semicolon. A return statement may appear in a function only if the function's
return type is int.

* A conditional statement looks like this:

    ```
    if (exp) {
      statements
    }
    else {
      statements
    }
    ```

    It consists of the keyword 'if', a boolean-valued expression enclosed in
parentheses, a list of zero or more statements enclosed in braces, the keyword
``else'', and a list of zero or more statements enclosed in braces. The else
part is optional, however.

* A while statement looks like this:

    ```
    while (exp) {
      statements
    }
    ```

    It consists of the keyword 'while', a boolean-valued expression enclosed in
parentheses, and a list of zero or more statements enclosed in braces.

There are many kinds of exps:

 * A previously-declared variable is an integer-valued expression.
 * An integer constant is an integer-valued expression.
 * Compound expressions of the form
    * `exp + exp`
    * `exp - exp`
    * `-exp`
   are integer-valued expressions.
 * Compound expressions of the form
    * `exp > exp`
    * `exp < exp`
    * `exp >= exp`
    * `exp <= exp`
    * `exp == exp`
    * `exp != exp`
   are boolean-valued expressions.

# Sample Programs 

This function returns the sum of its two parameters.

```
int add (int a, int b) {
  return a+b;
}
```

This function returns the larger of its two parameters.

```
int largest (int a, int b) {
  if (a > b) {
    return a;
  }
  else {
    return b;
  }
}
```

This function orders the three parameters into ascending order.

```
void sort (int a, int b, int c) {
  int temp = 0;
  if (a > b) {
    temp = a;
    a = b;
    b = temp;
  }
  if (b > c) {
    temp = b;
    b = c;
    c = temp;
  }
  if (a > b) {
    temp = a;
    a = b;
    b = temp;
  }
}
```
