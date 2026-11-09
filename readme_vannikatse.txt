README
Data and code related to manuscript:
Individual development of Gladiolus imbricatus followed up in a tray experiment over 5 years.
by Xenia Uffrecht, Rebeka Järve, Marite Kattai, Lauri Laanisto, Marika Kose
2027


#########################################
Codefiles

vannikatse_analysis.Rmd
vannikatse_analysis.html

################################################
vannikatse_data.csv

Years: 2022, 2023, 2024, 2025, 2026
3 trays with 30 bulbs each: 
A = replanted as one-leaved (towards the house)
B = replanted as two-leaved (in the middle)
C = replanted as flowering  (towards the treeline)

Between taking out the bulbs and planting them was 2 months. So they dried (Taken out in July beginning and replanted in September.)
The coordinates of the individuals were taken in 2022 at the time of the first measurements.
In the consequent years, data collection i.e. measuring of individuals and their sidebulbs went fast in tray B (~1h) and C (~45min), as individuals are easy to refind, while tray A (~3-4h) was less orderly and plants harder to refind.

X,Y [mm] are the individual coordinates, with the lower right corner as the nullpoint (when standing with the house to the right and the trees to the left. Either are far a away from the tray, which are positioned right next to each other. No difference between the trays by environment are expected)

Original datasheets  vannikatse_data.csv				Explanation

X (mm)					x							x-coordinate (short side of the tray)
Y (mm)					y							y-coordinate (long side of the tray)
						year						year of data collection (2022, 2023, 2024, 2025, 2026)
Isend					ID							ID code (Letter for tray A, B, C; number for individual 1-30; X1(-5) for newly found individuals. Then letter a(bcde) for sidebulb; and nr (1-12) for sidebulb orientation around the motherplant)
VEG kõrgus				vegh 						vegetative height (height of the heighest leaf, as all other height or length measurements measured with a measuring tape)
GEN kõrgus				genh 						generative height (height of the inflorescence tip, when stretched out)
1L kõrgus				h1 							height of first leaf (always equal to vegh)
1L pikkus				l1 							length of first leaf (=h1 for 1-leaved individuals, smaller than h1 for individuals with 1+ leaves)
1L laius				w1 							width of first leaf  (width measured at the widest point, with a ruler)

2L kõrgus				h2 							height of second leaf
2L pikkus				l2 							length of second leaf
2L laius				w2 							width of second leaf

3L kõrgus				h3 							height of third leaf
3L pikkus				l3 							length of third leaf
3L laius				w3							width of third leaf

Õisiku pikkus			flower_len  				flower length (start to measure at the first flower, straighten the inflorescence. Lauri had an image about it)
Õite arv				flower_nr					flower number (all flowers counted, even tiny ones. Measurement was done in end of June always, therefore we could not know, if those plants could grow larger)
KVõs arv				sidebulb_nr	 				number of new sidebulbs produced that year
Kvõs kaugus (mm)		sidebulb_dist				distance of the respective sidebulb (not noted for every sidebulb)
						previous_year_dist			distance of the respective sidebulb from the previous year.

4L kõrgus				h4 							height of fourth leaf
4L pikkus				l4 							length of fourth leaf
4L laius				w4 							width of fourth leaf

Teise õisiku pikkus		flower2_len					second flower length
Õite arv				flower2_nr					flower number

5L kõrgus				h5 							height of fifth leaf
5L pikkus				l5 							length of fifth leaf
5L laius				w5 							width of fifth leaf

w_20seeds				w_20s						weight of 20 randomized seeds [g]
w_1seed					w_1s						weight of 1 seed, from the previous measure /20 [g]
w_allseeds				w_allseeds					weight of all seeds in the respective inflorescence [g]
nr_seeds				nr_seeds					estimated number of seeds (w_allseeds/w_1s)
ProtocollFollowed		ProtFoll					followed the protocol of measuring the flower stalk (starting by the first bottom flower) = 1, compared to also measuring more of the stalk = 0
infl_length				infl_len 					length of inflorescence (mm)
nr_fl					nr_fl						number of flowers 
nr_fl_developed			nr_fl_dev 					number of flowers developed into seed mature capsules
w_20seeds2	w_1seed2	w_allseeds2	nr_seeds2	protocolFollowed2	infl_length2	nr_fl2	nr_fl_developed2 		 same thing as before, only for a second flower if available



Some special information had to be removed due to the table structure. The interesting thing here is, that those individuals had two inflorescences, not just one:
Information from original data sheet is missing from 2025 - C1 - Flower_length and number: 119 JA 45 and 10 JA 2
Information from original data sheet is missing from 2025 - C6 - Flower_length and number: 222 JA 70 and 14 JA 5

changes in the datasheet:
- A10 had kvos arv/kvos kaugus for two individuals, but no sidebulbs. But A11 had, so I moved that information down
- C14c4 had no kvos arv indicated in 2026. added 1 here. 
- went through all the individuals with sidebulbs and made sure, that kvos arv only said a number when there was a new sidebulb. Each existing sidebulb got its number in kvos kaugus(mm)
--> for most of the sidebulbs, I copied their distance from previous years to into the row that the sidebulb (and not the motherplant) was in
- changed ID names of sidebulbs (e.g. A17a in 2022 to A17a6, as in all other years. They didn't match before)
- A17a(6) had two different distances, 14/15 and 21mm. But are they different? I changed A17a to A17a6, but that might be wrong
- changed A11a and A11b to A11a6 and A11b9 (in 2022)
- changed A24a and A24b to A24a4 and A24b9 (in 2022)
- changed A8a, A8b, A8c to A8a6, A8b9, A8c3 (in 2022)

- change B10a to B10a2 (in 2022)
- change B25a to B25a3. B25b does not appear again, so I didn't add a compass number (the numbers behind the sidebulbs indicate what direction from the motherplant the sidebulb appeared, like a clock)
- change B3a to B3a12. 

changes in the script vannikatse hypotheses 2
- automated the plotflow into folders for the family tree


suspicious sidebulbs
A11d3 started with 374mm height
B8a6 started with 471mm height and flowering in 2026
B2a3 same
C6a8 same

C29c4 started with 483mm height
C6a8 started with 508mm height
AND MORE (haven't checked them all)


#########################################
Questions to the data:
1. Life stage transitions

2. Sidebulbs 
2.1 What is the survival rate of sidebulbs? (More frequent occurrence than 1 year)
2.2 How much does the production of sidebulbs depend on previous and/or current years' performance? And how much does it influence next years performance? 
		Performance = flowering or not, vegetative height (height of leaf 1), number of leaves 

3. Dormancy
3.1 What kind of dormancies exist?
3.2 In what context do they occur? (after flowering, all in the same year, after sidebulb)

4. leaf height differences
4.1.1 		1-leaf height difference (at 20cm, or else?)
4.1.2 		Can a sidebulb be distinguished from their mother?
4.2 	differences between the lowest leaves... 
