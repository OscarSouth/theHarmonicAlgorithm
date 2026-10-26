# Composer Catalogue

The graph holds harmonic transitions learned from over 10,000 compositions
by over 1,000 classical and jazz composers. This page breaks the two corpora
down, then lists every composer key carried by the classical (`:Cadence`)
graph and the syntax for using them. Keys are exactly as stored on the
graph's edge weights — this list is the authority for spelling.

## The corpora at a glance

| | classical — Yale Classical Archives Corpus | jazz — Jazz-Chord-Progressions-Corpus |
|---|---|---|
| compositions | 8,441 pieces | 2,614 tunes |
| composers | 488 | 679 with a sole credit (1,336 counting co-writers, who are mostly lyricists) |
| graph | `:Cadence`, 488 composer keys | `:Change`, 1,133 credit keys |
| concentration | the 13 most represented composers wrote half the pieces; median 3 pieces per composer, 174 composers with one | a long tail: the 20 most represented composers cover 29% of tunes, and half the tunes need 71 composers; median 1 tune per composer, 413 composers with one |

Together: 11,055 compositions by 1,161 composers. Six people appear in both
corpora (Corelli, Gershwin, Herbert, Lehár, Pachelbel, Romberg) and are
counted once.

**How each side is counted.** Classical: one piece per YCACL source file;
composers are the 488 keys left after slug curation merges the 494 source
folders (`anonymous` is one of them). Jazz: one tune per corpus file;
writers come from each tune's `ComposedBy` credit, split on *and*, *&*,
commas and slashes, excluding *Unknown* (15 tunes) and *Traditional* (21).
A composer is a writer with at least one sole credit (679); the 657 who only
ever appear as co-writers are counted separately, since most are lyricists
(Gus Kahn, Leo Robin, Sammy Cahn…). 87% of tunes include at least one
sole-credited composer. Four hyphenated credits are left unsplit, which makes
no material difference.

### Most represented

| classical | pieces | | jazz | tunes |
|---|---|---|---|---|
| Mozart | 685 | | Richard Rodgers | 66 |
| Bach | 652 | | Thelonious Monk | 60 |
| Scarlatti | 617 | | Harold Arlen | 51 |
| Beethoven | 372 | | Duke Ellington | 48 |
| Haydn | 359 | | Charlie Parker | 46 |
| Schubert | 223 | | John Coltrane | 45 |
| Telemann | 223 | | Wayne Shorter | 42 |
| Chopin | 204 | | Cole Porter | 42 |
| Brahms | 199 | | Horace Silver | 40 |
| Handel | 191 | | Irving Berlin | 35 |
| Sor | 174 | | Miles Davis | 35 |
| Vivaldi | 167 | | George Gershwin | 34 |

### Shape

- **Classical, by period** (8,204 dated pieces, 1548–1968, median 1798):
  before 1700 5.5% · 1700s 46.6% · 1800s 42.0% · 1900s 5.8%.
- **Classical, by composer nationality:** Germany 28.6% · Austria 18.0% ·
  Italy 17.4% · France 8.5% · Spain 4.3% · England 4.3% · Russia 4.2%.
  Period and nationality come from the YCAC metadata, which matches 8,305 of
  the 8,441 pieces.
- **Jazz, by credits per tune:** one writer 80% · two 15% · three or more 5%.

### Jazz keys

The jazz graph's 1,133 keys are one per credit line, normalised by
`normalizeComposer`, so co-writers share a key
(`billystrayhornandmiltonraskin`). `genJ` matches keys by substring, so one
name reaches every credit that contains it: `"strayhorn"` finds Billy
Strayhorn's solo and co-written credits alike. They are not catalogued here.

## Syntax

Composer specs are accepted anywhere a composer string is taken
(`harmonicContext`, `seek`, `genSilent`, ...):

```haskell
seek "*"                     -- aggregate: the whole corpus at once
seek "bach"                  -- one composer
seek "bach:30 debussy:70"    -- weighted blend (any number of names)
seek "none"                  -- offline: no graph, fallback generation only
```

Matching is case-insensitive (`"Bach"`, `"BACH"`, `"bach"` are identical).
Multi-word names use underscores exactly as listed below (`"strauss_ii"`,
`"de_falla"`).

## Available composers (488)

| | | | |
|---|---|---|---|
| `abel` | `chueca_y_robles` | `hofmannl` | `poole` |
| `accolay` | `cilea` | `hol` | `popp` |
| `adam` | `cimarosa` | `holcombe` | `poulenc` |
| `agnesi` | `clarke` | `holst` | `prokofiev` |
| `aguado` | `clementi` | `hough` | `prudent` |
| `ahlefeldt` | `clerambault` | `huber` | `pryor` |
| `albeniz` | `coleridgetaylor` | `hummel` | `puccini` |
| `albinoni` | `colkin` | `hummel_` | `purcell` |
| `albrechtsberger` | `cons` | `humperdinck` | `rachmaninov` |
| `alexandrov` | `cooke` | `hunten` | `raff` |
| `alkan` | `corbetta` | `ilynsky` | `rameau` |
| `ambroise` | `corelli` | `jackson` | `rance` |
| `anna_amalia` | `coste` | `jacquet_de_la_guerre` | `ravel` |
| `anonymous` | `couperin` | `jadassohn` | `ravina` |
| `archer` | `cramer` | `jensen` | `redmer` |
| `arensky` | `croft` | `joplin` | `reger` |
| `arne` | `crusell` | `joseffy` | `reicha` |
| `arnold` | `cui` | `jullien` | `reinecke` |
| `arriaga` | `czerny` | `juon` | `respighi` |
| `arrieta` | `da_costa` | `kalkbrenner` | `reubke` |
| `ascher` | `dandrieu` | `kelerbela` | `rheinberger` |
| `astorga` | `daquin` | `kerll` | `rieding` |
| `attwood` | `daragona` | `kessler` | `rietz` |
| `auber` | `dargomizhsky` | `ketelbey` | `rimskykorsakov` |
| `audran` | `dauprat` | `ketterer` | `rodriguez` |
| `bach` | `dawes` | `khachaturian` | `roman` |
| `bachcpe` | `de_diesbach` | `kiel` | `romberg` |
| `bachjc` | `de_falla` | `kircher` | `roncalli` |
| `bachman` | `de_koven` | `kirkpatrick` | `rosas` |
| `bachwf` | `de_macque` | `klendel` | `rossini` |
| `badarzewska` | `de_vilbac` | `kodaly` | `rubinstein` |
| `balakirev` | `debali` | `komarowski` | `saintluc` |
| `balfe` | `debussy` | `kopylov` | `saintsaens` |
| `banister` | `delgado_palacios` | `koschat` | `salieri` |
| `bargiel` | `delibes` | `kowalski` | `sambucetti` |
| `bargy` | `delioux` | `krafft` | `sammartini` |
| `barnard` | `delius` | `kraus` | `sanz` |
| `barnett` | `dentella` | `kreisler` | `sarasate` |
| `baron` | `denza` | `kreutzer` | `satie` |
| `barret` | `devienne` | `krieger` | `scarlatti` |
| `barrios` | `diabelli` | `krommer` | `scharwenka` |
| `bartok` | `dittersdorf` | `krov` | `scheidler` |
| `bazin` | `donizetti` | `kuhlau` | `schenker` |
| `bazzini` | `doppler` | `kuhnau` | `schmelzer` |
| `beethoven` | `dornell` | `kunzen` | `scholtz` |
| `bellini` | `draeseke` | `labitzky` | `schubert` |
| `bellinzani` | `dreyshock` | `lachner` | `schulhoff` |
| `bellman` | `dukas` | `lacombe` | `schulz` |
| `bembo` | `duparc` | `lajarte` | `schumann` |
| `benda` | `dupre` | `lalo` | `scott` |
| `benoit` | `durand` | `lassen` | `scriabin` |
| `berens` | `durango` | `lauffensteiner` | `sgambati` |
| `berlioz` | `durante` | `lawes` | `shand` |
| `berthomieu` | `dusek` | `leclair` | `shostakovich` |
| `berwald` | `duval` | `lecuona` | `sibelius` |
| `best` | `duvernoy` | `lefebure` | `sibencanin` |
| `bevilacqua` | `dvorak` | `lefebvre` | `sinding` |
| `billema` | `eberlin` | `legnani` | `smart` |
| `billi` | `eberling` | `lehar` | `smetana` |
| `billings` | `eccles` | `lemire` | `smith` |
| `bizet` | `eichner` | `lemmens` | `sokolov` |
| `blavet` | `elgar` | `leo` | `soler` |
| `bloch` | `ewald` | `leonarda` | `somis` |
| `blodek` | `exaudet` | `leschetizky` | `sor` |
| `blow` | `fasch` | `levy` | `sousa` |
| `boccherini` | `faure` | `leybach` | `souza` |
| `bochsa` | `ferrer` | `liguori` | `spindler` |
| `boehm` | `fibich` | `lindberg` | `spohr` |
| `boellmann` | `field` | `linley` | `stainer` |
| `boely` | `finger` | `liobet` | `stamitz` |
| `bohm` | `fiocco` | `liszt` | `stanely` |
| `boieldieu` | `fiorillo` | `lobo` | `stanford` |
| `boismortier` | `fischer` | `loeillet` | `steffani` |
| `bonis` | `fisher` | `losy` | `steiner` |
| `borodin` | `flies` | `lotti` | `strauss` |
| `bortnyansky` | `fomin` | `loud` | `strauss_i` |
| `bouffil` | `franck` | `luigini` | `strauss_ii` |
| `boulanger` | `frederick_ii_the_great` | `lully` | `stravinsky` |
| `bourgaultducoudray` | `freixanet` | `lyadov` | `strozzi` |
| `boyce` | `friese` | `macdowell` | `sullivan` |
| `braga` | `froberger` | `mahler` | `suppe` |
| `braham` | `fucik` | `malats` | `taffanel` |
| `brahms` | `gade` | `marais` | `taki` |
| `brant` | `galliard` | `mendelssohn` | `talexy` |
| `braunschweig` | `galuppi` | `mozart` | `tallard` |
| `brenton` | `ganne` | `naderman` | `tarrega` |
| `brescianello` | `gaultier` | `nascimbeni` | `tartini` |
| `broca` | `gelinek` | `nazareth` | `tchaikovsky` |
| `bruch` | `geminiani` | `nebra_blasco` | `telemann` |
| `bruckner` | `geoffroy` | `nedbal` | `thalberg` |
| `burgmuller` | `german` | `nicolai` | `thomas` |
| `busoni` | `gershwin` | `nicolini` | `torelli` |
| `buxtehude` | `giardini` | `nielsen` | `translateur` |
| `byrd` | `gibbons` | `novacek` | `tulou` |
| `cabanilles` | `gibel` | `nunes_garcia` | `umlauf` |
| `caldara` | `gigout` | `ocarolan` | `vanhal` |
| `campion` | `gimenez` | `offenbach` | `veracini` |
| `campra` | `giordani` | `oliveira` | `verdi` |
| `cano` | `giuliani` | `onslow` | `vierne` |
| `cantallos` | `glazunov` | `pachelbel` | `villalobos` |
| `carcassi` | `gliere` | `pacini` | `vinci` |
| `cardillo` | `glinka` | `paderewski` | `visee` |
| `carey` | `gluck` | `paganini` | `vitali` |
| `carissimi` | `godard` | `paisiello` | `vivaldi` |
| `carr` | `godowsky` | `paradis` | `wagenseil` |
| `carreno` | `haberbier` | `paradisi` | `wagner` |
| `carulli` | `haeffner` | `parker` | `walckiers` |
| `casadesus` | `halevy` | `parry` | `waldteufel` |
| `casanovas` | `halvorsen` | `pasquini` | `walker` |
| `casciolini` | `handel` | `pergolesi` | `walthew` |
| `casella` | `haslinger` | `perosi` | `walton` |
| `castello` | `hasse` | `persichetti` | `weber` |
| `catel` | `haydn` | `pescetti` | `weckmann` |
| `cernohorsky` | `haydnjm` | `peter` | `weiss` |
| `cervantes` | `heller` | `petersonberger` | `wesley` |
| `chabrier` | `henselt` | `pieczonka` | `widor` |
| `chambonnieres` | `herbert` | `pierpont` | `wieniawski` |
| `chaminade` | `herold` | `pixis` | `willis` |
| `chapi` | `herz` | `platti` | `witt` |
| `charpentier` | `hewitt` | `pleyel` | `wolf` |
| `cherubini` | `hoffmeister` | `ponce` | `wolff` |
| `chopin` | `hofmannj` | `ponchielli` | `zipoli` |

## Known key warts

Keys mirror the artefact's raw composer strings, warts included — kept
verbatim so this list stays the authority for what a query will match:

- `hummel` and `hummel_` are the same composer split by a trailing space in
  the source metadata (queued for a merge in the next artefact pass).
- `strauss`, `strauss_i` and `strauss_ii` need disambiguation against the
  source tree (family attribution) in the same pass.

## Musical character & density

*Deferred: per-composer harmonic analysis (signature cadences, dissonance
profile, modal tendencies) and corpus density (pieces, slices, edge counts —
how strongly each composer's voice is attested) are the subject of a separate
written analysis, published outside the release cycle.*
