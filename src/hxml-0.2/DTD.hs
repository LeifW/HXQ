----------------------------------------------------------------------------
--
-- Module	: HXML.DTD
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: provisional
-- Portability	: portable
--
-- CVS  	: $Id: DTD.hs,v 1.6 2002/10/12 01:58:57 joe Exp $
--
----------------------------------------------------------------------------
--
-- | Data types for SGML and XML document type definitions.
-- This is based on the SGML property set found in the DSSSL spec,
-- section 9.6.
--
-- History:
-- 	[23 Jan 2000], taken from earlier work, [4 Jan 1997]
--

module DTD where

import XML
import qualified AssocList as FM

type GI		= Name		-- generic identifier (element type name)
type DCN	= Name		-- data content notation name 

-- | Content expression, parameterized over type of primitive tokens
data CE a =
      Prim	  a		-- ^ Primitive content token
    | Rep 	 (CE a)		-- ^ Zero or more, '*' occurrence indicator
    | Opt  	 (CE a)		-- ^ Optional, '?' occurrence indicator
    | Plus  	 (CE a)		-- ^ One or more, '+' occurrence indicator
    | Seq	[(CE a)]	-- ^ Sequence, ',' connector
    | Or 	[(CE a)]	-- ^ Alternation, '|' connector
    | And	[(CE a)]	-- ^ Permutation, '&' connector
    deriving Eq

data PrimitiveToken =
      PCDATA			-- ^ Parsed character data
    | ELEMENT GI		-- ^ Element
    deriving Eq

type ModelGroup = CE PrimitiveToken
data CONTYPE = 		-- (element) content type
      DC_EMPTY 		-- ^ declared content (also: CDATA, RCDATA in SGML)
    | DC_ANY		-- ^ "ANY"
    | DC_MODELGRP ModelGroup
    deriving Show

data ELEMTYPE = ELEMTYPE {	--   element type definition
    gi		:: GI,		-- ^ generic identifier
    contype	:: CONTYPE,	-- ^ content type
    omissibility:: (Bool,Bool),	-- ^ omitstrt+omitend
    inclusions	:: [GI],
    exclusions	:: [GI] } deriving Show

-- Missing: attdefs, srmap(nm); all from DTGABS

data ATT_TYPE =		-- (dcltype/decl value type)
      ATcdata
    | ATentity
    | ATentities
    | ATid
    | ATidref
    | ATidrefs
    | ATnmtoken			-- %%% or name/number/nutoken
    | ATnmtokens		-- %%% or names/numbers/nutokens
    | ATnotation [DCN]		-- List of notation names
    | ATenumerated [Name]	-- nmtkgrp / name token group
  deriving Show

data ATT_DV = 	-- attribute default value (dflttype/default value type)
      ADVfixed String		-- "#FIXED ..."
    | ADVrequired		-- "#REQUIRED"
    | ADVimplied		-- "#IMPLIED"
    | ADVdefault String		-- "..."
    -- SGML only:
    | ADVcurrent		-- "#CURRENT"
    | ADVconref			-- "#CONREF"
  deriving Show

data ATTDEF = ATTDEF {	-- attribute definition
    att_name	:: Name,
    att_type	:: ATT_TYPE,
    att_dv 	:: ATT_DV } deriving Show

type ATTSPEC = (Name,String)	-- (attasgn/attribute assignment)


-- 
-- Entities:
--

type ExternalID = (Maybe PUBID, Maybe SYSID)
type PUBID = String
type SYSID = String

data ENTTYPE = 		-- entity type
      ETtext		-- SGML text entity
    | ETcdata
    | ETsdata
    | ETndata
    | ETsubdoc
    | ETpi		-- processing instruction entity

data EntityText =
      EN_INTERNAL String	 -- entity.text/replacement text 
    | EN_EXTERNAL ExternalID     -- entity.extid/external identifier
	deriving Show

data Entity = Entity {
    ename :: Name,		-- name
    etype :: ENTTYPE,		-- enttype/entity type
    etext :: EntityText,	-- see above
    edcn  :: Maybe DCN,		-- notname/notation name
    eatts :: [ATTSPEC]		-- atts/attributes
}

type EntityMap 		= FM.FM Name EntityText
predefinedEntities 	:: EntityMap
predefinedEntities	= foldr (uncurry FM.insert) FM.empty predefinedGEs
    where
	(==>)		= \a b -> (a,EN_INTERNAL b)
	predefinedGEs	= [
				"lt"	==> "<",
				"amp"	==> "&",
				"gt"	==> ">",
				"apos"	==> "'",
				"quot"	==> "\"" ]
-- 
-- Utility routine, used by scanner:
--

expandInternalEntity :: EntityMap -> Name -> Maybe String
expandInternalEntity entities name = 
    case FM.lookupM entities name of
	Just (EN_INTERNAL text)	-> Just text
	_			-> Nothing

--
-- DTDS:
--
data DTD = DTD {
    elements :: FM.FM Name ELEMTYPE,		-- elemtps / element types
    attlists :: FM.FM Name [ATTDEF], 		-- elemtype.attdefs
    genents  :: FM.FM Name EntityText,		-- general entities
    parments :: FM.FM Name EntityText,		-- parameter entities
    notations:: [DCN],				-- nots/notations
    dtdname  :: Name 				-- name (document type name)
} 	deriving Show

emptyDTD :: DTD
emptyDTD = DTD {
    elements = FM.empty,
    attlists = FM.empty,
    genents  = predefinedEntities,
    parments = FM.empty,
    dtdname  = "",
    notations= []
} 

declareParameterEntity,declareGeneralEntity :: Name -> EntityText -> DTD -> DTD
declareParameterEntity name entityText dtd =
	dtd { parments = FM.insertWith keepOld name entityText (parments dtd) }
	where keepOld old _new = old
declareGeneralEntity   name entityText dtd =
	dtd { genents  = FM.insertWith keepOld name entityText (genents  dtd) }
	where keepOld old _new = old

-- %%% DEAL WITH DUPLICATE DEFINITIONS HERE:
declareElements :: [GI] -> (Bool,Bool) -> CONTYPE -> ([GI],[GI]) -> DTD -> DTD
declareElements elementNames omissibility contentDefinition (incl,excl) dtd =
	dtd { elements = foldl mkElement (elements dtd) elementNames }
	where mkElement fm gi = FM.insert gi el fm where	
		el = ELEMTYPE {
			gi = gi,
			contype = contentDefinition,
			omissibility = omissibility,
			inclusions = incl,
			exclusions = excl
		    }

-- %%% DEAL WITH DUPLICATES:
declareAttlist :: [GI] -> [ATTDEF] -> DTD -> DTD
declareAttlist elementNames attdefs dtd =
	dtd { attlists = foldl addAttdefs (attlists dtd) elementNames }
	where addAttdefs fm gi = FM.insert gi attdefs fm

declareNotation :: DCN -> ExternalID -> DTD -> DTD
declareNotation dcn _unused dtd =
	dtd { notations = dcn : notations dtd }

-- Need srmaps::Dict[SRASSOC]+usemaps::Dict{-GI-}srmap(nm)|elemtype.srmap(nm)
-- notation: name, extid, attdefs

instance Show PrimitiveToken where
  showsPrec _ PCDATA	= showString "#PCDATA"
  showsPrec _ (ELEMENT gi) = showString gi

instance (Show prim) => Show (CE prim) where
  showsPrec _ mg = pp mg where
    pp (Prim p)	= shows p
    pp (Rep x)	= shows x . showString "*"
    pp (Opt x)	= shows x . showString "?"
    pp (Plus x)	= shows x . showString "+"
    pp (Seq x)	= showgroup ", " x
    pp (Or x)	= showgroup " | " x
    pp (And x)	= showgroup " & " x
    showgroup delim l	= showString "(" . showl l . showString ")" where
	showl [x]	= shows x
	showl (x:xs)	= shows x . showString delim . showl xs
	showl []	= showString "-- ERROR: empty model group! --"

-- EOF --
