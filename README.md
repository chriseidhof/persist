Persist
=======

Persist is an abstraction layer in Haskell over storage systems such as
relational databases, nosql databases and more. Persists includes a code
generator that takes your model and generates a Haskell interface for it.
Persist comes with multiple backends, eventually. For now there is only an SQLite backend.

Example
============

You start by defining your entities and relationships. An entity is simply a
datatype name with attributes, and a relationship is a description of how your
entities relate to each other.

Step 1: Create your schema
--------------------------

The first step is to define your entities. In this example, we will build a
database for a simple quiz system. Each quiz has a number of questions, which
all have a title and three options to choose from. When someone takes a quiz, a
response is stored in the database.

    data Response = Response {
      name    :: String,
      email   :: String,
      date    :: Date,
      answers :: [Int]
    }

    data Quiz = Quiz {
      description :: String,
      subject     :: String
    }

    data Question = Question {
      title   :: String,
      choiceA :: String,
      choiceB :: String,
      choiceC :: String
    }

Now we will define the relationships:

    relationship questions = Quiz <1-*> Question

    relationship responses = Quiz <1-*> Response

Step 2: Generate the corresponding code
---------------------------------------

Put all the code from step 1 in a file called `examples/Model.phs` and execute the command `persist
examples/Model.phs -o examples/Model.hs`. This will generate a file called `Model.hs` with all the necessary
code to continue. 

In particular, the generated file will contain the following things:

* The original datatype definitions

* Template Haskell code to derive [regular](http://hackage.haskell.org/package/regular) instances

* A value of type `Relationship a b` for each relationship between entities `a` and `b`.

* A function `createEntity` for each entity. This function not only asks for the
  entity value, but also for references to relationships. 
  
  For example, for the `Question` entity, the type is `createQuestion :: Persistent db => Question -> Ref Quiz -> db (Ref Question)`.
  This means that you have to provide a question and a reference to the `Quiz`,
  and it will return a reference to the newly created `Question` entity. 

* A function `createSchema` that creates the database schema for you.

All the code in the file is backend-independent.

Step 3: Use the code
--------------------

* Now you are ready to use the generated code. Have a look at examples/Test.lhs to see what's possible, or browse the haddock documentation.

History
=======

Many of the ideas in this library come from my [thesis](http://github.com/chriseidhof/thesis/downloads) work.
In the library described in my thesis I tried to prevent offline code generation, which resulted in a lot of type-level functions. In turn, this resulted in very messy errors when you did something wrong. This library is an experiment in finding the sweet spot between generated code, type safety and user friendliness.
