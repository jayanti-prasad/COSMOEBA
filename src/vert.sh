cat fort.22  | awk '{if($2==1)printf("%s %s %s %s %s %s %s %s\n",$1,$3,$4,$5,$6,$7,$8,$9)}' > v1.out 
cat fort.22  | awk '{if($2==2)printf("%s %s %s %s %s %s %s %s\n",$1,$3,$4,$5,$6,$7,$8,$9)}' > v2.out 
cat fort.22  | awk '{if($2==3)printf("%s %s %s %s %s %s %s %s\n",$1,$3,$4,$5,$6,$7,$8,$9)}' > v3.out 
cat fort.22  | awk '{if($2==4)printf("%s %s %s %s %s %s %s %s\n",$1,$3,$4,$5,$6,$7,$8,$9)}' > v4.out 
cat fort.22  | awk '{if($2==5)printf("%s %s %s %s %s %s %s %s\n",$1,$3,$4,$5,$6,$7,$8,$9)}' > v5.out 
cat fort.22  | awk '{if($2==6)printf("%s %s %s %s %s %s %s %s\n",$1,$3,$4,$5,$6,$7,$8,$9)}' > v6.out 
cat fort.22  | awk '{if($2==7)printf("%s %s %s %s %s %s %s %s\n",$1,$3,$4,$5,$6,$7,$8,$9)}' > v7.out 

