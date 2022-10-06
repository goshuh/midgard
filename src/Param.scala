package midgard

import  chisel3.util._


case class Param(
  en:       Boolean = true,
  fsSkip:   Boolean = false,
  bsSkip:   Boolean = false,

  vaBits:   Int     = 64,
  maBits:   Int     = 64,
  paBits:   Int     = 48,
  clBits:   Int     = 512,

  attrBits: Int     = 4,
  asidBits: Int     = 16,

  tlbEn:    Boolean,
  tlbWays:  Int,

  vlbIdx:   Int,
  vlbWays:  Int,

  llcIdx:   Int,

  mlbEn:    Boolean,
  mlbSets:  Int,
  mlbWays:  Int,

  ptcEn:    Boolean,
  ptcWays:  Seq[Int],

  mrqWays:  Int,

  deqWays:  Int,

  ctlBase:  BigInt,
  ctlSize:  BigInt,
  ctlInit:  BigInt  = 0,

  dbg:      Boolean = false) {

  val clBytes    =  clBits / 8
  val clWid      =  log2Ceil(clBytes)

  val vdnBits    =  vaBits - 3
  val mdnBits    =  maBits - 3
  val pdnBits    =  paBits - 3

  val vcnBits    =  vaBits - clWid
  val mcnBits    =  maBits - clWid
  val pcnBits    =  paBits - clWid

  val vpnBits    =  vaBits - 12
  val mpnBits    =  maBits - 12
  val ppnBits    =  paBits - 12

  val ptwLvl     = (mpnBits + (9      - 1)) / 9
  val ptwTop     =  mpnBits - (ptwLvl - 1)  * 9

  val mlbIdx     =  log2Ceil(mlbSets)
  val mlbTagBits =  mpnBits - mlbIdx

  val mrqIdx     =  log2Ceil(mrqWays)

  val deqIdx     =  log2Ceil(deqWays)

  require(ptwLvl == ptcWays.size)
  require(mrqIdx >= deqIdx)
}