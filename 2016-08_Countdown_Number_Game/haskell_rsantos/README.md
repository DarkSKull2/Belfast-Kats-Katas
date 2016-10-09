ISSUES / Improvement Opportunities 
	- total blind brute force
		- prune Leaf Tree for (*) and (+) since the order left and right branches do not care
		- only uses length 6, needs to also search with small lengths


in my laptop ( Intel(R) Celeron(R) CPU 847 @ 1.10GHz ) 
	time ./fkata-bin [1,2,3,4,5,6] 1081
	((4*(2+1))*((5*6)*3))= 1080.0
	goal=1081 [delta=1.0]

		real   2m37.561s
		user   2m34.866s
		sys    0m0.515s

in [https://www.tutorialspoint.com/compile_haskell_online.php]
	time ./Main [1,2,3,4,5,6] 1081
	((4*(2+1))*((5*6)*3))= 1080.0
	goal=1081 [delta=1.0]

		real   0m38.816s
		user   0m38.502s
		sys    0m0.335s