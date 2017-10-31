 select name, id, sizename from product order by id;

local:

update product set imagename = 'zoguard/ZoGuard-Plus-small-dog.jpg' where id = '10';
update product set imagename = 'zoguard/ZoGuard-Plus-medium-dog.jpg' where id = '11';
update product set imagename = 'zoguard/ZoGuard-Plus-large-dog.jpg' where id = '12';
update product set imagename = 'zoguard/ZoGuard-Plus-xlarge-dog.jpg' where id = '13';

update product set imagename = 'zoguard/ZoGuard-Plus-cat-2.jpg' where id = '26';
update product set imagename = 'zoguard/ZoGuard-Plus-cat-2.jpg' where id = '4';
update product set imagename = 'zoguard/ZoGuard-Plus-cat-2.jpg' where id = '5';

update product set imagename = 'adventure/Adventure-Plus-small-dog.jpg' where id = '6';
update product set imagename = 'adventure/Adventure-Plus-medium-dog.jpg' where id = '7';
update product set imagename = 'adventure/Adventure-Plus-large-dog.jpg' where id = '8';
update product set imagename = 'adventure/Adventure-Plus-xlarge-dog.jpg' where id = '9';

update product set imagename = 'adventure/Adventure-Plus-small-cat.jpg' where id = '1';
update product set imagename = 'adventure/Adventure-Plus-medium-cat.jpg' where id = '2';
update product set imagename = 'adventure/Adventure-Plus-large-cat.jpg' where id = '3';

update product set imagename = 'shieldtec/ShieldTec-Plus-small-dog.jpg' where id = '14';
update product set imagename = 'shieldtec/ShieldTec-Plus-medium-dog.jpg' where id = '15';
update product set imagename = 'shieldtec/ShieldTec-Plus-large-dog.jpg' where id = '16';
update product set imagename = 'shieldtec/ShieldTec-Plus-xlarge-dog.jpg' where id = '17';

update product set imagename = 'frontline/Frontline-Plus-small-dog.jpg' where id = '18'; 
update product set imagename = 'frontline/Frontline-Plus-medium-dog.jpg' where id = '19'; 
update product set imagename = 'frontline/Frontline-Plus-large-dog.jpg' where id = '20'; 
update product set imagename = 'frontline/Frontline-Plus-xlarge-dog.jpg' where id = '21'; 

update product set imagename = 'frontline/Frontline-Plus-cat.jpg' where id = '25';
update product set imagename = 'frontline/Frontline-Plus-cat.jpg' where id = '22';
update product set imagename = 'frontline/Frontline-Plus-cat.jpg' where id = '24';


dev:

update product set imagename = 'zoguard/ZoGuard-Plus-small-dog.jpg' where id = '10';
update product set imagename = 'zoguard/ZoGuard-Plus-medium-dog.jpg' where id = '11';
update product set imagename = 'zoguard/ZoGuard-Plus-large-dog.jpg' where id = '12';
update product set imagename = 'zoguard/ZoGuard-Plus-xlarge-dog.jpg' where id = '13';

update product set imagename = 'zoguard/ZoGuard-Plus-cat-2.jpg' where id = '1';
update product set imagename = 'zoguard/ZoGuard-Plus-cat-2.jpg' where id = '4';
update product set imagename = 'zoguard/ZoGuard-Plus-cat-2.jpg' where id = '5';

update product set imagename = 'adventure/Adventure-Plus-small-dog.jpg' where id = '6';
update product set imagename = 'adventure/Adventure-Plus-medium-dog.jpg' where id = '7';
update product set imagename = 'adventure/Adventure-Plus-large-dog.jpg' where id = '8';
update product set imagename = 'adventure/Adventure-Plus-xlarge-dog.jpg' where id = '9';

update product set imagename = 'adventure/Adventure-Plus-medium-cat.jpg' where id = '2';
update product set imagename = 'adventure/Adventure-Plus-large-cat.jpg' where id = '3';

update product set imagename = 'shieldtec/ShieldTec-Plus-small-dog.jpg' where id = '14';
update product set imagename = 'shieldtec/ShieldTec-Plus-medium-dog.jpg' where id = '15';
update product set imagename = 'shieldtec/ShieldTec-Plus-large-dog.jpg' where id = '16';
update product set imagename = 'shieldtec/ShieldTec-Plus-xlarge-dog.jpg' where id = '17';

update product set imagename = 'frontline/Frontline-Plus-small-dog.jpg' where id = '18'; 
update product set imagename = 'frontline/Frontline-Plus-medium-dog.jpg' where id = '19'; 
update product set imagename = 'frontline/Frontline-Plus-large-dog.jpg' where id = '20'; 
update product set imagename = 'frontline/Frontline-Plus-xlarge-dog.jpg' where id = '21'; 

update product set imagename = 'frontline/Frontline-Plus-cat.jpg' where id = '22';
update product set imagename = 'frontline/Frontline-Plus-cat.jpg' where id = '23';

prod

update product set sizename = 'Small' where size_c = '0';
update product set sizename = 'Medium' where size_c = '1';
update product set sizename = 'Large' where size_c = '2';

update product set sizename = 'Small' where size_c = '3';
update product set sizename = 'Medium' where size_c = '4';
update product set sizename = 'Large' where size_c = '5';
update product set sizename = 'X-Large' where size_c = '6';

update product set sizename = 'Small' where size_c = '7';
update product set sizename = 'Medium' where size_c = '8';
update product set sizename = 'Large' where size_c = '9';
update product set sizename = 'X-Large' where size_c = '10';

update product set sizename = 'Small' where size_c = '11';
update product set sizename = 'Medium' where size_c = '12';
update product set sizename = 'Large' where size_c = '13';
update product set sizename = 'X-Large' where size_c = '14';

 id |          name           | size_c
----+-------------------------+--------
  2 | Adventure Plus for Cats |      1
  3 | Adventure Plus for Cats |      2
  4 | ZoGuard Plus for Cats   |      1
  5 | ZoGuard Plus for Cats   |      2
  6 | Adventure Plus for Dogs |      3
  7 | Adventure Plus for Dogs |      4
  8 | Adventure Plus for Dogs |      5
  9 | Adventure Plus for Dogs |      6
 10 | ZoGuard Plus for Dogs   |      7
 11 | ZoGuard Plus for Dogs   |      8
 12 | ZoGuard Plus for Dogs   |      9
 13 | ZoGuard Plus for Dogs   |     10
 14 | ShieldTec Plus for Dogs |     11
 15 | ShieldTec Plus for Dogs |     12
 16 | ShieldTec Plus for Dogs |     13
 17 | ShieldTec Plus for Dogs |     14
  1 | ZoGuard Plus for Cats   |      0
 18 | Frontline Plus for Dogs |      7
 19 | Frontline Plus for Dogs |      8
 20 | Frontline Plus for Dogs |      9
 21 | Frontline Plus for Dogs |     10
 22 | Frontline Plus for Cats |      0
 23 | ZoGuard Plus for Cats   |      0
 24 | Frontline Plus for Cats |      1
 25 | Frontline Plus for Cats |      2


update product set imagename = 'zoguard/ZoGuard-Plus-small-dog.jpg' where id = '10';
update product set imagename = 'zoguard/ZoGuard-Plus-medium-dog.jpg' where id = '11';
update product set imagename = 'zoguard/ZoGuard-Plus-large-dog.jpg' where id = '12';
update product set imagename = 'zoguard/ZoGuard-Plus-xlarge-dog.jpg' where id = '13';

update product set imagename = 'zoguard/ZoGuard-Plus-cat-2.jpg' where id = '1';
update product set imagename = 'zoguard/ZoGuard-Plus-cat-2.jpg' where id = '4';
update product set imagename = 'zoguard/ZoGuard-Plus-cat-2.jpg' where id = '5';

update product set imagename = 'adventure/Adventure-Plus-small-dog.jpg' where id = '6';
update product set imagename = 'adventure/Adventure-Plus-medium-dog.jpg' where id = '7';
update product set imagename = 'adventure/Adventure-Plus-large-dog.jpg' where id = '8';
update product set imagename = 'adventure/Adventure-Plus-xlarge-dog.jpg' where id = '9';

update product set imagename = 'adventure/Adventure-Plus-medium-cat.jpg' where id = '2';
update product set imagename = 'adventure/Adventure-Plus-large-cat.jpg' where id = '3';

update product set imagename = 'shieldtec/ShieldTec-Plus-small-dog.jpg' where id = '14';
update product set imagename = 'shieldtec/ShieldTec-Plus-medium-dog.jpg' where id = '15';
update product set imagename = 'shieldtec/ShieldTec-Plus-large-dog.jpg' where id = '16';
update product set imagename = 'shieldtec/ShieldTec-Plus-xlarge-dog.jpg' where id = '17';

update product set imagename = 'frontline/Frontline-Plus-small-dog.jpg' where id = '18'; 
update product set imagename = 'frontline/Frontline-Plus-medium-dog.jpg' where id = '19'; 
update product set imagename = 'frontline/Frontline-Plus-large-dog.jpg' where id = '20'; 
update product set imagename = 'frontline/Frontline-Plus-xlarge-dog.jpg' where id = '21'; 

update product set imagename = 'frontline/Frontline-Plus-cat.jpg' where id = '22';
update product set imagename = 'frontline/Frontline-Plus-cat.jpg' where id = '24';
update product set imagename = 'frontline/Frontline-Plus-cat.jpg' where id = '25';

