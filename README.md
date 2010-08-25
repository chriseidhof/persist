Persist
=======

Persist is an abstraction layer in Haskell over storage systems such as
relational databases, nosql databases and more. Persists includes a code
generator that takes your model and generates a Haskell interface for it.
Persist comes with multiple backends: a sqlite backend and a couchdb backend.

Example
============

You start by defining your entities and relationships. An entity is simply a
datatype name with attributes, and a relationship 

Step 1: Create your schema
--------------------------

The first step is to define your entities. In this example, we will build a
database for a simple quiz system. Each quiz has a number of questions, which
all have a title and three options to choose from. When someone takes a quiz, a
response is stored in the database.

> data Response = Response {
>   name    :: String,
>   email   :: String,
>   date    :: Date,
>   answers :: [Int]
> }

> data Quiz = Quiz {
>   description :: String,
>   subject     :: String
> }

> data Question = Question {
>   title   :: String,
>   choiceA :: String,
>   choiceB :: String,
>   choiceC :: String
> }

Now we will define the relationships:

> questions ::: Quiz <1-*> Question
> responses ::: Quiz <1-*> Response

Step 2: Generate the corresponding code
---------------------------------------

Put all the code from step 1 in a file called schema.phs and execute the command `persist
schema.phs`. This will generate a file called Schema.hs with all the necessary
code to continue.


Step 3: Use the code
--------------------
