module Data.Persist.Compile (compile) where

import Data.Persist.AST
import Language.Haskell.Exts.Syntax
import Data.Either (partitionEithers)

compile :: [Either Decl Relationship] -> [Decl]
compile input = let (decls, relationships) = partitionEithers input
                    relationshipsBothDirections = relationships ++ (map reverseRelationship relationships)
                    dbClass = UnQual (Ident "Persistent")
                in concat [ decls
                          , compileRelationships decls relationships
                          , concatMap (createMethod dbClass relationshipsBothDirections) decls
                          ]

compileRelationships :: [Decl] -> [Relationship] -> [Decl]
compileRelationships _ _ = [] -- todo

createMethod :: QName -> [Relationship] -> Decl -> [Decl]
createMethod dbClass rs  d = 
  [ TypeSig noLoc [funName] (TyForall Nothing ctx $ TyFun (typ d) (f (TyApp monad (TyApp refType (typ d)))))
  , FunBind [Match noLoc funName ((PVar (Ident "value")):(map (PVar . Ident . snd) relArgs)) Nothing (UnGuardedRhs (rhs relArgs) ) (BDecls [])]
  ]
  where ctx     = [ClassA dbClass [monad]]
        monad   = TyVar (Ident "db")
        funName      = Ident $ "create" ++ datatypeName 

        relArgs = zip rels (map relVarName [1..])
        relVarName x = "x" ++ show x

        datatypeName = name d
        rels    = involvedRelationships datatypeName rs

        f = relationshipsToFun rels

rhs relArgs = Do $ concat 
  [ [ Generator noLoc (PVar (Ident "i")) (App (var "create") (var "value")) ]
  , concatMap relCreate relArgs
  , [ Qualifier $ App (var "return") (var "i") ]
  ]
  where relCreate :: (Relationship, String) -> [Stmt]
        relCreate (r,s) = [ addRelation r s ]
        addRelation r s | reversed r == False = Qualifier $ App (App (var "addRelation") (var "i")) (var s)
                        | otherwise           = Qualifier $ var "addRelation" `App` var s `App` var "i" `App` var (relName r)
                        

var :: String -> Exp
var = Var . UnQual . Ident

relationshipsToFun :: [Relationship] -> (Type -> Type)
relationshipsToFun []     = id
relationshipsToFun (x:xs) = TyFun (TyApp refType $ TyCon (UnQual (Ident (relToName x)))) . relationshipsToFun xs

involvedRelationships :: String -> [Relationship] -> [Relationship]
involvedRelationships d = filter (\r -> relFromName r == d && isToOne r)

typ :: Decl -> Type
typ (DataDecl _ _ _ nm  _ _ _) = TyCon (UnQual nm)
typ _ = error "Compile.typ"


refType :: Type
refType = TyCon (UnQual (Ident "Ref"))

name :: Decl -> String
name (DataDecl _ _ _ (Ident nm)  _ _ _) = nm
name s = error $ "Compile.name" ++ show s

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0
