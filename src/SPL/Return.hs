{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module SPL.Return where

import SPL.AST
import SPL.Colors (blue, red, yellow)
import SPL.Parser.Parser (SourceSpan, showEnd, showStart)

{-
    Did it return?
    Dit it return a value?
    No return and Without are compatible = Right ...
    Return value and (No return or Without value) is imcompatible = Left String
    return Value and return value is competable = Right ...

 -}

data TypeOfRet = WithValue SourceSpan | WithoutValue SourceSpan | No -- No is only for if there are no return statements. Only the base case should make it
-- Only return statements should span the With and Without values

class ReturnCheck a where
  returns :: [a] -> Either String TypeOfRet

instance ReturnCheck (Stmt ParsedP) where
  returns [] = Right No
  returns (ReturnStmt meta Nothing : _) = Right (WithoutValue meta)
  returns (ReturnStmt meta (Just _) : _) = Right (WithValue meta)
  returns (IfStmt meta _ consequent Nothing : later) =
    case (returns consequent, returns later) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right No, Right No) -> Right No
      (Right (WithoutValue _), Right (WithoutValue m)) -> Right (WithoutValue m)
      (Right (WithValue _), Right (WithValue m)) -> Right (WithValue m)
      (Right (WithoutValue _), Right No) -> Right No
      (Right No, Right (WithoutValue m)) -> Right (WithoutValue m)
      --- Start of the left
      (Right (WithValue m), Right No) ->
        Left $
          red "Invalid returns"
            ++ "in if condition at "
            ++ showStart meta
            ++ "\nThe true case of the condition returns at "
            ++ showStart m
            ++ " but the rest of the code does not.\n"
            ++ "Each branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value to end of the code or remove the return statement at "
            ++ showEnd m
      (Right (WithValue m1), Right (WithoutValue m2)) ->
        Left $
          red "Invalid returns"
            ++ " in if condition at "
            ++ showStart meta
            ++ "\nThe true case of the condition returns at "
            ++ showStart m1
            ++ " but a return statement later at "
            ++ showStart m2
            ++ " does not.\n"
            ++ "Each branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value to the return statement at "
            ++ showEnd m2
            ++ " or remove the value from the return statement in the if at "
            ++ showEnd m1
      (Right No, Right (WithValue m)) -> Right (WithValue m)
      (Right (WithoutValue m1), Right (WithValue m2)) ->
        Left $
          red "Invalid returns"
            ++ " in if condition at "
            ++ showStart meta
            ++ "\nThe true case of the condition returns without a value at "
            ++ showStart m1
            ++ " but a return statement later at "
            ++ showStart m2
            ++ " returns with a value.\n"
            ++ "Each branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value to the return statement in the if at "
            ++ showEnd m1
            ++ " or just remove that return statement."
            ++ "\nYou can also remove the return value from "
            ++ showEnd m2
            ++ "."
  returns (IfStmt meta _ consequent (Just alternative) : later) =
    case (returns consequent, returns alternative, returns later) of
      -- Just propagate the left
      (Left err, _, _) -> Left err
      (_, Left err, _) -> Left err
      (_, _, Left err) -> Left err
      -- At this point there is no left anywere and the two first ones have the same right if not No
      (Right (WithValue m1), Right (WithoutValue m2), Right (WithValue m3)) ->
        Left $
          red "Invalid returns"
            ++ " in if condition at "
            ++ showStart meta
            ++ "\nThe else case of the condition returns at "
            ++ showStart m2
            ++ " but "
            ++ red "without"
            ++ " a value while the true case of the if condition and later in the function you return with a value at "
            ++ showStart m1
            ++ " and at "
            ++ showStart m3
            ++ "\nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value to the else part at "
            ++ showEnd m2
            ++ " or remove the return value from the return in the true part of the if condition at "
            ++ showEnd m1
            ++ " and at "
            ++ showEnd m3
            ++ "."
      (Right (WithoutValue m1), Right (WithValue m2), Right (WithoutValue m3)) ->
        Left $
          red "Invalid returns"
            ++ " in if condition at "
            ++ showStart meta
            ++ "\nThe else case of the condition returns a value at "
            ++ showStart m2
            ++ " while in the if case and later in the function you return without a value at "
            ++ showStart m1
            ++ " and at "
            ++ showStart m3
            ++ "\nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value to the true case of the if condition at "
            ++ showEnd m1
            ++ " and at "
            ++ showEnd m3
            ++ " or remove the return value from the return in the else part at "
            ++ showEnd m2
            ++ "."
      (Right (WithoutValue m1), Right (WithoutValue m2), Right (WithValue m3)) ->
        Left $
          red "Invalid returns"
            ++ " in if condition at "
            ++ showStart meta
            ++ "\nThe if and else case of the condition return at "
            ++ showStart m1
            ++ " and "
            ++ showStart m2
            ++ " but "
            ++ red "without"
            ++ "a value while later in the function you return with a value at "
            ++ showStart m3
            ++ "\nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value to the true case of the if condition at "
            ++ showEnd m1
            ++ " and in the else part at "
            ++ showEnd m2
            ++ " or remove the value from the return at "
            ++ showEnd m3
            ++ "."
      (Right (WithValue m1), Right (WithoutValue m2), Right (WithoutValue m3)) ->
        Left $
          red "Invalid returns"
            ++ " in if condition at "
            ++ showStart meta
            ++ "\nThe if case of the condition returns a value at "
            ++ showStart m1
            ++ " while in the else and later in the function you return without a value at "
            ++ showStart m2
            ++ " and "
            ++ showStart m3
            ++ "\nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither remove the return value from the true part of the if condition at "
            ++ showEnd m1
            ++ " or add a return value to the else part at "
            ++ showEnd m2
            ++ " and at "
            ++ showEnd m3
            ++ "."
      -- This is just an early return that is ok but we save the last one to compare with
      (Right (WithoutValue _), Right (WithoutValue _), Right (WithoutValue m)) -> Right (WithoutValue m)
      -- In this case both if return without a value, no and without value are fine here but withvalue is bad
      (Right (WithoutValue _), Right (WithoutValue _), r@(Right No)) -> r
      (Right No, Right No, Right m) -> Right m
      (Right No, Right (WithoutValue _), Right (WithoutValue m)) -> Right (WithoutValue m)
      (Right No, Right (WithValue _), Right (WithValue m)) -> Right (WithValue m)
      (Right No, Right (WithoutValue m1), Right (WithValue m2)) ->
        Left $
          red "Invalid return "
            ++ "at "
            ++ showStart m1
            ++ ". In the else you return but"
            ++ red "without"
            ++ " a value while later at "
            ++ showStart m2
            ++ " you do return a value."
            ++ "\nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a value in the else part at "
            ++ showEnd m1
            ++ " or remove the return value at "
            ++ showEnd m2
            ++ "."
      (Right (WithoutValue _), Right No, Right (WithoutValue m)) -> Right (WithoutValue m)
      (Right (WithValue _), Right No, Right (WithValue m)) -> Right (WithValue m)
      (Right (WithoutValue m1), Right No, Right (WithValue m2)) ->
        Left $
          red "Invalid return "
            ++ "at "
            ++ showStart m1
            ++ ". In the if you return but "
            ++ red "without"
            ++ " a value while later at "
            ++ showStart m2
            ++ " you do return a value."
            ++ "\nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value in the true case of the if condition at "
            ++ showEnd m1
            ++ " or remove the return value at "
            ++ showEnd m2
            ++ "."
      (Right (WithValue m1), Right No, Right (WithoutValue m2)) ->
        Left $
          red "Invalid return"
            ++ ". In the if at "
            ++ showStart m1
            ++ " you return with a value while later at "
            ++ showStart m2
            ++ " you return without value. "
            ++ "\nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither remove the return value in the true case of the if condition  at "
            ++ showEnd m1
            ++ " or add a return value at "
            ++ showEnd m2
            ++ "."
      (Right (WithValue m), Right No, Right No) ->
        Left $
          red "Invalid return"
            ++ ". In the if at "
            ++ showStart meta
            ++ " you return a value at "
            ++ showStart m
            ++ " but no where else you return a value. \nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither remove the return statement at "
            ++ showStart m
            ++ " or add a return statement at the end of the function."
      (Right (WithoutValue _), Right No, Right No) -> Right No
      (Right No, Right (WithoutValue _), Right No) -> Right No
      (Right No, Right (WithValue m1), Right (WithoutValue m2)) ->
        Left $
          red "Invalid return"
            ++ ". In the if at "
            ++ showStart meta
            ++ " you return a value at "
            ++ showStart m1
            ++ " but no where else you return a value. \nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither remove the return statement at "
            ++ showStart m1
            ++ " or add a return value to the return at "
            ++ showEnd m2
            ++ " the end of the function."
      (Right No, Right (WithValue m), Right No) -> Left $ red "Invalid return." ++ " In the else case of the if at " ++ showStart meta ++ " you return a value at " ++ showStart m ++ " but you do not return a value in the true case of this if statement nor after the if statement. \nEach branch should return the same way. You should either always return a value or never. \nThis return statement might be a mistake. Try to remove it. If not add a return statement to the end of this block."
      --- These onese catch more things
      (Right (WithValue m1), Right (WithoutValue m2), _) ->
        Left $
          red "Invalid returns"
            ++ " in the if condition at "
            ++ showStart meta
            ++ "\nThe true case of the if condition returns a value at "
            ++ showStart m1
            ++ " while the else case does not at"
            ++ showStart m2
            ++ " \nEach branch should return the same way. You should either always return a value or never."
            ++ " You probably either forgot a return value in the else or you forgot to remove the return in the true case of the if condition "
            ++ " Add a return value at "
            ++ showEnd m2
            ++ " or remove it at "
            ++ showEnd m1
      (Right (WithoutValue m1), Right (WithValue m2), _) ->
        Left $
          red "Invalid returns"
            ++ " in the if condition at "
            ++ showStart meta
            ++ "\nThe true case of the condition returns without a value at "
            ++ showStart m1
            ++ " while the else case returns with a value at."
            ++ showStart m2
            ++ " \nEach branch should return the same way. You should either always return a value or never."
            ++ " You probably forgot the return value in the true case of the if or you forgot to remove the return from the else case"
            ++ " Either add a return value at "
            ++ showEnd m1
            ++ " or remove the return at "
            ++ showEnd m2
      -- We were here! -- idk about this one
      -- I think we just be save and do all of them, but is this ok or not?  Each branch returns?
      -- I think we just be save and do all of them, but is this ok or not?  Each branch returns. I do want to detect not returning a value though
      (Right (WithValue m1), Right (WithValue m2), Right (WithoutValue m3)) ->
        Left $
          red "Invalid returns"
            ++ " in the if condition at "
            ++ showStart meta
            ++ ". Both cases in the if condition return a value but later on at "
            ++ showEnd m3
            ++ " you have a return without a value."
            ++ "\nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value at "
            ++ showEnd m3
            ++ " or remove the return statements at "
            ++ showEnd m1
            ++ " and "
            ++ showEnd m2
            ++ "."
      (Right (WithValue m1), Right (WithValue m2), Right No) ->
        Left $
          red "Invalid returns"
            ++ " in the if condition at "
            ++ showStart meta
            ++ ". Both cases in the if condition return a value but later on at you don't return a value.\n"
            ++ "Each branch should return the same way. You should either always return a value or never.\n"
            ++ yellow "This return error is actually more a sign of dead code because we return already everywhere so the last one does not matter"
            ++ "\nEither add a "
            ++ " return value at the end of this function  "
            ++ " or remove the return statements at "
            ++ showEnd m1
            ++ " and "
            ++ showEnd m2
            ++ "."
      -- todo This one might be wrong as well because we go for the retrun at the end, which is one we might actually never get to?
      (Right (WithValue _), Right (WithValue _), Right (WithValue m)) -> Right (WithValue m)
  returns (WhileStmt meta _ body : later) =
    case (returns body, returns later) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right No, Right No) -> Right No
      (Right (WithoutValue _), Right (WithoutValue m)) -> Right (WithoutValue m)
      (Right (WithValue _), Right (WithValue m)) -> Right (WithValue m)
      (Right (WithoutValue _), Right No) -> Right No
      (Right No, Right (WithoutValue m)) -> Right (WithoutValue m)
      --- Start of the left
      (Right (WithValue m), Right No) ->
        Left $
          red "Invalid returns"
            ++ " in while loop at "
            ++ showStart meta
            ++ "\nThe while loop returns at "
            ++ showStart m
            ++ " but the rest of the code does not.\n"
            ++ "Each branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value to end of the code or remove the return statement at "
            ++ showEnd m
            ++ " from the while statement."
      (Right (WithValue m1), Right (WithoutValue m2)) ->
        Left $
          red "Invalid returns"
            ++ " in while loop at "
            ++ showStart meta
            ++ "\nThe while loop returns at "
            ++ showStart m1
            ++ " but a return statement later at "
            ++ showStart m2
            ++ " does not."
            ++ "\nEach branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value to the return statement at "
            ++ showEnd m2
            ++ " or remove the value from the return statement in the while loop at "
            ++ showEnd m1
      (Right No, Right (WithValue m)) -> Right (WithValue m)
      (Right (WithoutValue m1), Right (WithValue m2)) ->
        Left $
          red "Invalid returns"
            ++ " in while loop at "
            ++ showStart meta
            ++ "\nThe true case of the while loop returns without a value at "
            ++ showStart m1
            ++ " but a return statement later at "
            ++ showStart m2
            ++ " returns with a value.\n"
            ++ "Each branch should return the same way. You should either always return a value or never."
            ++ "\nEither add a return value to the return statement in the while loop at "
            ++ showEnd m1
            ++ " or remove the return value from "
            ++ showEnd m1
  returns ((AssignStmt {}) : later) = returns later
  returns ((ExprStmt _ _) : later) = returns later
  returns (BlockStmt _ : _) = error "Encountered block statement! There should not be any BlockStmt in the return check phase!!!"

instance ReturnCheck (Decl ParsedP) where
  returns [] = Right No
  returns (FunDecl _ funname _ _ _ body : later) = either addFunctionName Right (returns body) >> returns later
    where
      addFunctionName err = Left $ "In function " ++ blue funname ++ ": " ++ err
  returns (VarDecl {} : later) = returns later