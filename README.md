# GenSolver

## Build Status

Mono | .NET
---- | ----
[![Mono CI Build Status](https://img.shields.io/travis/halcwb/GenSolver/master.svg)](https://travis-ci.org/halcwb/GenSolver) | [![.NET Build Status](https://img.shields.io/appveyor/ci/halcwb/GenSolver/master.svg)](https://ci.appveyor.com/project/halcwb/GenSolver)


## Background

GenSolver aims to solve a set of product and/or sum equations using the following general format:

> Product Equation: y = x1 * x2  </br>
> Sum Equation x1 = x3 + x4 

A variable in an equation can also occur in another equation. So, for example ***x1*** can be found in both the product equation as in the sum equation. 
These equations can be solved by giving each variable a value. When an equation contains n variables, then the equation can be solved when n-1 variables 
have a value.

For example, when **x3 = 1**, **x4 = 1** and **x2 = 3** then it follows that **x1** becomes **2** and **y** becomes **6**. Vice versa when **y = 6**, **x1 = 2** 
and **x3 = 1** then **x4** is **1** and **x2** is **2**.
  
GenSolver can solve any combination of product and/or sum equations independent of the order in which variables are set.

But there is more. Each variable in an equation has the following properties:

* a name
* a list of possible values
* a minimum
* a maximum and
* an increment

A variable can be considered as a range of possible values. The following notation can be used to describe a variable:

> **< ... >** meaning a variable that encompasses all rational numbers </br>
> **< min.. >** a variable restricted by values **> min** </br>
> **[ min.. >** a variable restricted by values **>= min** </br>
> **< ..n.. >** a variable in which each value is a multiple of **n** </br>
> **< ..max >** a variable restricted to values **< max** </br>
> **< ..max ]** a variable restricted to values **<= max**

So, in the aforementioned example, the variables actually are set to *lists* of values which happen to contain just one value. Therefore, the following scenario can also be 
solved by GenSolver:

> **y = [ 12 ]** meaning that the only possible value in the y variable is **12**, i.e. **y = 12** </br>
> **x2 = [ 1..1.. >** meaning a variable with only values **>= 1** and an increment of **1** and </br>
> **x1 = [ 1..1.. >** a variable of values with only values **>= 1** and an increment of **1**.

It can be derived that each possible value in both **x1** and **x2** must be a divisor of **12**. Solving the equation:

> **y [ 12 ] = x1 [ 1..1.. > * x2 [ 1..1.. >**

Results in 

> **x1 = [ 1,2,3,4,6,12 ]** and </br>
> **x2 = [ 1,2,3,4,6,12 ]**

Another example is a sum equation in which each variable has a *minimum* **> 0** like:

> **y < 0.. > = x1 < 0.. > + x2 < 0.. > + x3 < 0.. >**

When the minimum of, for example, **x2** is set to **1** then **y** will be at least **1** (as x1 and x3 will be > 0).

By specifying variables as lists of possible values with either a lower or upper boundary and/or a fixed increment, solving a sum or 
a product equation results in calculating the restrictions there are for each variable in the equation of possible values that will 
result in solving the equation. Given a sum equation or a product equation the starting point is that each variable in the equation
can be set to any rational number:

> **y < ... > = x1 < .. > * x2 < ... >** </br>
> **x1 < ... > = x3 < .. > + x4 < ... >**

When for example the lower limit of **x3** is set to **1**, then:

> **y < ... > = x1 < ... > * x2 < ... >** </br>
> **x1 < ... > = x3 [ 1.. > + x4 < ... >**

This doesn't change (restrict) the rest of the variables. Also when the minimum of **x2** is set to greater than **1**, the rest 
of the variables remain untouched and still can take any value:

> **y < ... > = x1 < ... > * x2 < 1.. >** </br>
> **x1 < ... > = x3 [1 .. > + x4 < ... >**

However, when in this equation the lower boundary of **x4** is set to **2**, then the rest of the variables will be restricted as well:

> **y < 3.. > = x1 [ 3.. > * x2 < 1.. >** </br>
> **x1 [ 3.. > = x3 [ 1.. > + x4 [2 .. >**

Suddenly, both **x1** and **y** cannot take on any value anymore but are restricted to values **>= 3** and **> 3**, respectively. 

GenSolver thus ensures that in any combination of product and/or sum equations as long as only one variable at a time is restricted and 
the resulting restrictions in other variables are respected, there is always a solution where each variable in the equations can just 
have one possible value, i.e. all the equations are in a *solved state*.

A more elaborate example and output of GenSolver looks like this:

 


## Setup



## Building
The build can be started by running:

* `build.sh` on unix systems (tested on max os x)
* `build.cmd` on windows systems

Running `build.cmd` or `./build.sh` is the same as running the command with the `ALL` argument. To list all build targets use the `--listTargets` or `-lt` argument.

