## warning import not supported
from random import randint

## function declaration without return
def f(x):
 5

a = f(3)

## function declaration with return
def g(x,y):
   x
   return 8

## call g
g(5,6)

g(8,9)

## bad call format
#g(1)
#g(1,2,3,4)

## return oustide function
#return 5

# function modifying global variable
x = 5

def h(t,u):
   #global x
   #o = x #NameError
   x = 4
   i = x
   return i

x = 42

## variable not modified
x = 5
y = x

## toplevel if-type # !! TODO don't expect return !!
if type(5) == "int":
   7
else:
   8

## in function if-type # !! TODO find stmt.Sreturn !!
def i(x):
   if type(x) == int:
      return 9
   else:
      return 10
