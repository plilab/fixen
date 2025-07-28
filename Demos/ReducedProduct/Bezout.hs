-- Copyright (c) 2014, Abdelwaheb Miled

-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.

--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.

--     * Neither the name of Abdelwaheb Miled nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module ReducedProduct.Bezout (
-- *  Extended gcd of integers
besout ,
-- * Modular inverse of integer
inverseMod,
-- * shift
shift,
-- * pad
pad,
-- * trim
trim,
-- * trim'
trim' ,
-- * deg
deg,
-- * (+:)
(+:),
-- * mods
mods,
-- * mMod
mMod,
-- * Multiplication of polynomials in F_p[x]
multPolyZ,
-- * Euclidean division of polynomials for F_p[x]
euclideanPolyMod, 
-- * Extended gcd of polynomials in F_p[x]
extendedgcdpoly ,
-- * Inverse of polynomial P modulo polynomial Q in F_p[x]
inversePolyMod,
-- * Pretty form  polynom input
prettyFormPoly ) where

-- besout
-- | besout compute extended gcd of two integers. For example : "besout" 13 17 = [4,-3,1] , this means that 
-- gcd of 13 and 17 is 1 and 1 could be written as linear combination of 13 and 17 as 1 = 4*13 - 3*17.
besout :: Integer -> Integer -> [Integer]
besout x y = bBesout [1,0,x] [0,1,y]
	where
	bBesout u v = case v!!2 of 0 -> u
		    	           _ -> let q = div (u!!2) (v!!2) in bBesout v [u!!k - q * v!!k | k <- [0..2]]
-- inverseMod	
-- | From besout identity we derive the modular inverse of an integer x modulo an integer y . Integers
-- x and y must be relatively prime , otherwise "inverseMod" returns zero. For example : "inverseMod" 13 17 = 4 , "inverseMod" 17 13 = 10.
inverseMod :: Integer -> Integer -> Integer
inverseMod x y = let a = besout x y in
		case a!!2 of 1 -> mods (a!!0) y
			     _ -> 0
-- shift
-- | padding left with n zeros
shift n l = l ++ replicate n 0
-- pad
-- | padding right with n zeros
pad n l = replicate n 0 ++ l
-- | trim
trim x = dropWhile (== 0) x
-- | trim'
trim' x = let y = trim x in if y == [] then [0] else y
-- deg
-- | degree of polynomial 
deg l = length (trim l) - 1
-- | (+:) add or abstract two list of different length
(+:) op p q = let d = (length p) - (length q) in zipWith op (pad (-d) p) (pad d q)
-- mods
-- | mods return positve remainder of "mod" operator
mods :: Integer -> Integer -> Integer
mods x p = if y >= 0 then y else y + p where y = mod x p 
-- mMod
-- | mMod map "mods" over list of integers
mMod :: [Integer]-> Integer ->[Integer]
mMod [] _ = []
mMod (x:xs) p = (mods x p) : mMod xs p

-- multPolyZ
-- |multPolyZ compute the product of two polynomial P Q in F_p[x].  
-- Write ploynom P of degree n as a sequence of coefficients P = [an, a_(n-1),.., a_0].
--To compute the product of polynomials P,Q we borrow the Horner multiplication rules as described by the following chain.
--It consists to do n compositions of functions detailed in the following diagramm:
--Q -> anxQ + a_(n-1)Q
--
--	R -> xR + a_(n-2)Q
--
--		R -> xR + a_(n-3)Q
--
--			R -> xR + a-1Q
--
--				...
--
--					R -> xR + a_0Q
--
--Let f = [2,0,3,2,1::Integer], g = [2,5,-3,1::Integer] in F_7[x].
--"multPolyZ 7 f g" = [4,3,0,0,3,2,6,1] . 
-- That means that f*g = 4*x^7 + 3*x^6 + 3*x^3 + 2*x^2 + 6*x + 1  in F_7[x].
-- This function require writing polynoms in decreasing order .
multPolyZ :: Integer ->[Integer]->[Integer]-> [Integer]
multPolyZ _ [0] _ = [0]
multPolyZ _ _ [0] = [0]
multPolyZ p f [a] = mMod (map (* a) f) p
multPolyZ p [a] g = mMod (map (* a) g) p
multPolyZ p u v = trim' $! hHornP p u v where hHornP p u v = let t = inverseMod (head u) p in let  a = mMod (map (* t) u) p in mMod (map (* (head u)) $ hornP p (tail a) v v) p where hornP p u v z = hrnP p u v z 1 where 
hrnP p [] v _ _ = v
hrnP p (u:us) v z s = let w = mMod ( map (* u) z) p in let r = zipWith(+) (v ++ [0]) (pad s w) in hrnP p us r z (s + 1)

-- euclidanPolyMod
-- | "euclidanPolyMod" compute the quotient and remainder of euclidean division of polynomial P by
--  Q in the ring F_p[x] where p is a prime number.
-- Let f = [2,0,3,2,1::Integer], g = [2,5,-3,1::Integer] in F_7[x].
-- "euclideanPolyMod" 7 f g = [[1,1],[1,4,0]].
-- That means f = (x + 1 )*g + (x^2 + 4*x ) in F_7[x].
euclideanPolyMod :: Integer -> [Integer]-> [Integer]-> [[Integer]]
euclideanPolyMod p f [1] = [mMod f p , [0] ]
euclideanPolyMod p f g  = let x = zPolyMod p (trim $! mMod f p) (trim $! mMod g p) in
			case x!!0 of [] -> [[0],x!!1]
				     _ -> case x!!1 of [] -> [x!!0 , [0]]
						       _ -> x	
	where
	zPolyMod p f g = let y = head g in if y == 1 then polyQuotRemMod p f g else let a = (inverseMod y p) in let g' = mMod ( map (* a) g) p in let qr = polyQuotRemMod p f g' in [mMod ( map (* a) $! (head qr)) p, last qr]
			where
				polyQuotRemMod _ f [] = [f,[]]
				polyQuotRemMod _ [] g = [[],g]
				polyQuotRemMod p f g = aux p (trim f) (trim g) []
	 	 			where aux p f s q | ddif < 0 = [q , f]
					    		  | otherwise = aux p f' s q'
	     	      					where ddif = (deg f) - (deg s)
		    	             			      a = mods (head f) p
		      	             			      gs = mMod (map (* a) $ shift ddif s) p
		      	             			      q' = mMod (trim $ (+:) (+) q $ shift ddif [a]) p
		       	             			      f' = mMod (trim $ tail $ (+:) (-) f gs) p


-- extendedgcdpoly
-- | "extendedgcdpoly" compute the extended gcd of polynomials P and Q in the ring F_p[x] where p is a prime number. 
-- Let f = [2,0,3,2,1::Integer], g = [2,5,-3,1::Integer] in F_7[x]. 
-- "extendedgcdpoly" 7 f g = [[5,3],[2,6,5],[2,1]] .
-- This means that the gcd of f and g in F_7[x] is the polynom 2*x+1 , and
--
-- 2 * x + 1 = (5 * x + 3) * f + (2 * x^2 + 6 * x + 5) * g in F_7[x].

extendedgcdpoly :: Integer -> [Integer] -> [Integer] -> [[Integer]]
extendedgcdpoly p x y = let t = besoutPoly p x y in let z = t!!2 in
			case length z of 1 -> let r = inverseMod (z!!0) p in [mMod (map (* (r)) $! (t!!k)) p | k <-[0..2]]
					 _ -> t
			where 
			besoutPoly p x y = besoutPoly' p [[1],[0],x] [[0],[1],y]
				where
				besoutPoly' p u v = if last v == [0] then u else let q = (head) $! euclideanPolyMod p (last u) (last v) in let z = ( (map (trim')) $! [let x = multPolyZ p q (v!!k) in mMod ((+:) (-) (u!!k) x ) p | k <-[0..2]]) in besoutPoly' p v z
-- inversePolyMod
-- | "inversePolyMod" compute the modular inverse of polynomial P(x) modulo Q(x) in F_p[x] . Polynomials 
-- P and Q must be relatively prime , otherwise it return [0].
-- Let f = [2,0,3,2,1::Integer], g = [2,5,-3,1::Integer] in F_13[x]. 
-- "extendedgcdpoly" 13 f g =[[7,3,7],[6,8,4,7],[1]].
-- So f and g are relatively prime in F_13[x] , and the inverse of f modulo g in F_13[x] is given by 
-- "inversePolyMod" 13 f g = [7,3,7] . 
--
-- Which says that the inverse of ploynomial f denoted f^(-1) is 7*x^2 + 3*x + 7 modulo g in F_13[x]
inversePolyMod :: Integer -> [Integer] -> [Integer] -> [Integer]
inversePolyMod p x y = let t = extendedgcdpoly p x y in let z = length $ t!!2 in 
			case z of 1 -> t!!0
				  _ -> [0]
-- prettyFormPoly
-- | This is a facility for writing non nul terms of polynomial , if f = 5*x^13 + 4*x^5 + (-3)*x^4 + 11*x + 19 , then
--
-- prettyFormPoly [[5,13],[4,5],[-3,4],[11,1],[19,0]] = [5,0,0,0,0,0,0,0,4,-3,0,0,11,19]

prettyFormPoly :: [[Integer]]->[Integer]
prettyFormPoly [[]] = []
prettyFormPoly [[a,b]]=[a]
prettyFormPoly (x:t:xs) = let z = [x!!0] ++ (replicate (fromIntegral (x!!1 - t!!1 - 1)) 0) in z ++ prettyFormPoly (t:xs)

