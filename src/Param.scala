package midgard

import  chisel3.util._


case class Param(
  en:       Boolean = true,

  vaBits:   Int     = 64,
  maBits:   Int     = 64,
  paBits:   Int     = 48,
  clBits:   Int     = 512,

  attrBits: Int     = 4,
  asidBits: Int     = 16,
  sdidBits: Int     = 16,

  pmtBits:  Int,

  tlbEn:    Boolean,
  tlbWays:  Int,

  vlbIdx:   Int,
  vlbWays:  Int,

  ttwNum:   Int,

  vscEn:    Boolean,
  vscSets:  Int,
  vscWays:  Int,

  vldSets:  Int,
  vldWays:  Int,
  dirBits:  Int,

  llcIdx:   Int,

  mlbEn:    Boolean,
  mlbWays:  Int,

  ptcEn:    Boolean,
  ptcWays:  Int,

  mrqWays:  Int,

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

  val vscBits    =  log2Ceil(vscSets)
  val vscTagBits =  mdnBits - vscBits

  val vldBits    =  log2Ceil(vldSets)
  val vldTagBits =  mdnBits - vldBits

  val ttwIdx     =  log2Ceil(ttwNum)
  val mlbIdx     =  log2Ceil(mlbWays)
  val ptcIdx     =  log2Ceil(ptcWays)
  val mrqIdx     =  log2Ceil(mrqWays)
}