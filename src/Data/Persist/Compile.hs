module Data.Persist.Compile (compile) where

import Data.Persist.AST
import Language.Haskell.Exts.Syntax
import Data.Either (partitionEithers)

compile :: [Either Decl Relationship] -> [Decl]
compile input = let (decls, relationships) = partitionEithers input
                    relationshipsBothDirections = relationships ++ (map reverseRelationship relationships)
                in concat [ decls
                          , compileRelationships decls relationships
                          , concatMap (createMethod relationshipsBothDirections) decls
                          ]

compileRelationships :: [Decl] -> [Relationship] -> [Decl]
compileRelationships _ _ = [] -- todo

createMethod :: [Relationship] -> Decl -> [Decl]
createMethod rs  d = let 
                         funName      = Ident $ "create" ++ datatypeName 
                   in
  [ TypeSig noLoc [funName] (TyForall Nothing ctx $ TyFun (typ d) (f (TyApp monad (TyApp refType (typ d)))))
  , FunBind [Match noLoc funName [] Nothing (UnGuardedRhs rhs) (BDecls [])]
  ]
  where ctx     = [ClassA dbClass [monad]]
        monad   = TyVar (Ident "m")
        dbClass = UnQual (Ident "Persistent")

        datatypeName = name d
        rels    = involvedRelationships datatypeName rs

        f = relationshipsToFun rels

        rhs = Var (UnQual (Ident "undefined"))

relationshipsToFun :: [Relationship] -> (Type -> Type)
relationshipsToFun []     = id
relationshipsToFun (x:xs) = TyFun (TyCon (UnQual (Ident (relToName x)))) . relationshipsToFun xs

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
