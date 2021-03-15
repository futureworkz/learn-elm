# Learn Elm!
Read `https://guide.elm-lang.org/`
with the below additional notes for each section

The focus is on mastering syntax first before learning The Elm Architecture.
Trainer is advised to constantly harp on reading the types of all functions
so students have a firm grasp on typing and reading types.

For all excerises, inform the student to create a new file in `/src/Excerise/NewFile.elm` and a corresponding test case in `/tests/Spec/Excerise/NewFile.elm`

## Introduction
- Open mind to learn a new language
- Explain the little program line by line
- Explain function update msg model
- Immutability
- No classes in Elm
- All functions are 100% PURE
- What is a Type? A type is a kind of contract between a variable and a compiler. It tells the compiler what is the kind of a variable and it allows the compiler to enforce rules upon the variable's kind eg. cannot call .length on an Int variable

## Core Language

### Values
Don't use the interactive on the website
Ask student to clone this repo
Ensure IDE is setup properly
Walkthrough `/tests/Tests.elm` 
and explain 
- module/import/export
- let..in 
- front/back piping
- `_ = Debug.log "title" value`

Introduce elm core reference:
`https://package.elm-lang.org/packages/elm/core/latest/String`
`https://elm-lang.org/docs/syntax`
Explain a -> b -> c and the concept of currying
Explain String vs Char

Play with an example `String.length`
Do the Strings.elm exercise

### Functions
### If-else
Do the FormatCents.elm exercise 
Do the Factorial.elm exercise

## Lists
Can only hold the same type for the entire list
List vs Array
Do the demo and kata in Lists.elm exercise

## Tuple
Maximum 3 values

## Records:
- Known as objects in Javascript
- Immutable
- Field type cannot be changed
Do User exercise `create` & `updateEmail`
Inform student in `create` that a record type is actually a function
Inform student to write their own test cases and function signatures

# Types
## Reading Types
- Primitives and Lists: Demo [1, "hello"]
- Functions: 
Explain a -> b -> c
String.padLeft : Int -> Char -> String -> String
String.all : (Char -> Bool) -> String -> Bool
- Type Annotation: 
We always annotate all functions and variables because it is a form of documentation
- Type Variables: 
Explain List.map : (a -> b) -> List a -> List b
Explain List.range : Int -> Int -> List Int
- Skip constrained type variables


## Type Aliases:
Use type alias Email = String in User exercise

## Custom Types
## Pattern Matching
Go through these 2 sections together
Do Api exercise 
- Teach student to expose Response(..)
- Tag String to Loaded and Error and return the string
Do Route exercise

Focus on the difference between Custom Type, Type variant, Type associated variant, Type variable, Type alias (also Record type)

# Error Handling
## Maybe
Do `findByName` & `findByEmail` in User Exercise
- Trainer to add Maybe Gender and show the compiler errors and fix it
Do `getFemaleUsers` in User Exercise

```elm
-- Discuss Maybe Type vs Custom Type
type Gender =
    Male
    | Female
    | Unknown

type alias User = {
    gender: Gender
}
```

## Result
Do only `validate` in User exercise
Guide students to think about function composition (in `create` function)

Hidden exercise:
Guide students to think about handling String as error
Write a function named errorMessage : ErrorCode -> String
where 
`type ErrorCode = InvalidEmail | InvalidName | InvalidAge`

- Making impossible state impossible: `https://www.youtube.com/watch?v=IcgmSRJHu_8`

# The Elm Architecture
## Buttons
Create a file `/src/Main.elm` and copy the example code
Run `npm start`
Student can view localhost:3000 to play with the app

Go through `https://package.elm-lang.org/packages/elm/html/latest/Html`
Do the exercise: Add a button to reset the counter to zero.

Go through the Elm Debugger.

Install `rtfeldman/elm-css`
and do some simple styling!
Reference: `https://package.elm-lang.org/packages/rtfeldman/elm-css/latest`

## Text Fields
This part can be skipped if the students are strong
Ask student to add the code in current app
so they can understand better

## Forms
Student may be confused about `onInput Name`
explain that `onInput Name` is equivalent to `onInput (\string -> Name string)`
and also similar to React `<input onChange={string => console.log(string)}`

Ask student to add the code in current app
so they can understand better

Ask student to add `validateName` from User exercise
and display an error message

# Commands and Subscriptions
## HTTP
Ask student to install `./node_modules/.bin/elm-app install elm/http`

Tell student to implement a button "Load Text"
and then it will call the HTTP to load the text.
`https://elm-lang.org/assets/public-opinion.txt`

Trainer do take note that this is a more challenging exercise.

Work with the student to create the ApiResponse type
```elm
type ApiResponse
  = Ready
  | Loading
  | Success String
  | Fail String
```

and explain how the entire process will be like first.

Skip JSON section and focus on Random and Time first
to allow student to have a firm grasp on `Cmd msg` and `Sub msg`

## Random
Ask student to install `./node_modules/.bin/elm-app install elm/random`
Discuss why Random is considered a side-effect?
Ask student to create a button that roll 1 to 6

## Time
Discuss why Time is considered a side-effect?
Discuss storing time as Posix vs String (`https://en.wikipedia.org/wiki/ISO_8601`)
Ask student to install `./node_modules/.bin/elm-app install elm/time`
Ask student to display time in millis int

Explain Task type in `Time.here`
Optional: Install `ryannhg/date-format` to format the date

## JSON
- Ask student to install `elm-app install elm/json`
- Do Json exercise
- Ask student to change the "Load Image" button to load the cat image and also to show more fields of the returned data
- Install `NoRedInk/elm-json-decode-pipeline` and use it to decode
- Using the Json exercise, encode the data back into a Json string

# JavaScript Interop
Go through it quickly

Watch Life of a File:
`https://www.youtube.com/watch?v=XpDsk374LDE&feature=youtu.be`

# Web Apps
## Navigation
Ask student to update the app to have the following pages:
- /
- /index
- /about
- /login
- /dashboard
Inform student to refer to their Route exercise

## URL Parsing

Go through the guide and documentation and ask student 
to update their route in the app to consume this library

## Modules
## Structure
To discuss with students
Using User exercise, share with student the idea of Data Type files

## Final Exercise (Optional):
- Login a user with an API
- Name, Email, Password, Gender (optional)
- Display emall is taken error
- Display user is registered successfully

* An API for Registeration?
https://exercism.io/tracks/elm

## Additional Readings
- https://elm-lang.org/docs/syntax
- Opaque Types: `https://medium.com/@ghivert/designing-api-in-elm-opaque-types-ce9d5f113033`
- Phantom Types: `https://thoughtbot.com/blog/modeling-currency-in-elm-using-phantom-types`
