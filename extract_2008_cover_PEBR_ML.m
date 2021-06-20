function extract_2008_cover_PEBR
% This is the GOOD version; can be used for .tiff or .bmp. Note that the 
% rotation in 45-49 and the filtering in 75 are optional (but it's good to
%use them).

% 9/16/19 rewritten for new extraction of images in order to get the
% following:
% - extract the area
% - find 90th percentile of sizes (in the biggest year across all years)
% - divide by five to get the size-classes
% all this is based on an older extractor, which is:
% 3/5/2017 for the Frontiers paper, BR, fixed image issue
% This needs to be copied into every working folder where
% extraction takes place as the same name as the function.
% New: 10x10 filter to avoid salt-and-pepper errors; 5x5 filter gives
% almost same result; but no filter gives erroneous smallest SC

%Note: new version for the paper with Pete. This version:
%- counts sizes of the corals and dumps them into areaPor and then Apo
%- end loop
%- then, of all corals across all transects the size classes are defined on
% 5 steps between the 0 and 90th percentile. MUST not be done with each
% transects (i.e. across years), else it makes no sense bc. the perentiles
% will always vary.

clear all;% always goo to clear the deck
n = dir('*.tif'); %NOTE: the folder in which the images sit must be the current directory
if isempty(n)
   disp('cd to a directory that has tifs')
   return
end

h = waitbar(0,n(1).name);%can be omitted, but nice to see progress
Size_Por=[];Size_Fav=[];Size_Pla=[];Size_Cyp=[];Size_Acr=[];Size_Cor=[];cover=[];
APo=[];AFa=[];APl=[];ACy=[];AAc=[];ACor=[];

for k= 1:length(n)
f0=n(k).name;
ppic=imread(f0);
%
%==>FOR TEST_RUNNING load just a single image==blind-out later again!!
ppic=imread('UAA T1 0419 copy.tif');
%==============================
if size(ppic,1)<size(ppic,2)%if image is horizontal
    pic=ppic;
else
    pic=imrotate(ppic,90);%turn it sideways
end
pic=imresize(pic, [(size(pic,1)*1000./size(pic,2)),1000]); %this keeps the correct length/height relationship 
%Note: The resizing should really be to 1000 pixels. But sometimes the
%incoming image resolution requires some doctoring; 5000 or 10000 are good
%sizes; 10000 makes it pretty slow
clear ppic;%just to free up internal memory

%Note: this now searches according to the color values of the blobs on the image
Acanthastrea =intersect(find(pic(:,:,1)==255), intersect(find(pic(:,:,2)==100), find(pic(:,:,3)==100)));%<=====CHANGE Porites harrisoni
Favites=intersect(find(pic(:,:,1)==0), intersect(find(pic(:,:,2)==0), find(pic(:,:,3)==255)));%<=====CHANGE Faviids
Platygyra=intersect(find(pic(:,:,1)==50), intersect(find(pic(:,:,2)==255), find(pic(:,:,3)==50)));%<=====CHANGE Platygyra
Anomastrea=intersect(find(pic(:,:,1)==150), intersect(find(pic(:,:,2)==50), find(pic(:,:,3)==50)));%<=====CHANGE Platygyra
Coscinarea=intersect(find(pic(:,:,1)==150), intersect(find(pic(:,:,2)==150), find(pic(:,:,3)==25)));%<=====CHANGE Platygyra
Cyph=intersect(find(pic(:,:,1)==10), intersect(find(pic(:,:,2)==150), find(pic(:,:,3)==138)));%<=====CHANGE Cyphastrea
Acropora=intersect(find(pic(:,:,1)==90), intersect(find(pic(:,:,2)==50), find(pic(:,:,3)==0)));%<=====CHANGE Acropora
Dipsastrea=intersect(find(pic(:,:,1)==255), intersect(find(pic(:,:,2)==0), find(pic(:,:,3)==0)));%<=====CHANGE Acropora

%this now turns each species layer into a black & white image
Plat=[Pla1];
Acro=[Acropora];
Coral=[Acanthastrea;Fa;Pla1;Anomastrea;Coscinarea;Cyph;Acropora;Dipsastrea];
l_b=zeros(size(pic(:,:,1)));
l_b1=l_b;l_b2=l_b;l_b3=l_b;l_b4=l_b;l_b5=l_b;l_b6=l_b;
l_b1(Acanthastrea )=255;l_b2(Fa)=255;l_b3(Plat)=255;l_b4(Cyph)=255;l_b5(Acro)=255;l_b6(Coral)=255; %this is the black layer
Por=l_b1;Fav=l_b2;Pla=l_b3;Cyp=l_b4;Acr=l_b5;Cor=l_b6;

%CRITICAL to reduce the salt-and-pepper error, especially in bmp's! can be
%omitted in some tiffs; makes the program run slow, but is worth it!
Por=medfilt2(Por,[10,10]);Fav=medfilt2(Fav,[10,10]);Pla=medfilt2(Pla,[10,10]);Cyp=medfilt2(Cyp,[10,10]);Acr=medfilt2(Acr,[10,10]);Cor=medfilt2(Cor,[10,10]);

% now count the areas for size/frequency
%[labeled1 num1] = bwlabel(Coral,8);%bwlabel labels all the spots in a 4 or 8 neighborhood returns in NUM the number of connected objects found in BW.
[labeledAcanthastrea numAcanthastrea] = bwlabel(Por,8);
[labeledFa numFa] = bwlabel(Fav,8);
[labeledPla1 numPla1] = bwlabel(Pla,8);
[labeledCyph numCyp] = bwlabel(Cyp,8);
[labeledAcr numAcr1] = bwlabel(Acr,8);
[labeledCoral numCoral] = bwlabel(Cor,8);

% convert binary image to label matrix one can also do: double(bw)
%data1=regionprops(labeled1,'area');%regionsprop also has the centroid function
dataPor=regionprops(labeledAcanthastrea,'area');%regionsprop also has the centroid function
dataFav=regionprops(labeledFav,'area');%regionsprop also has the centroid function
dataPla=regionprops(labeledPla1,'area');%regionsprop also has the centroid function
dataCyp=regionprops(labeledCyp,'area');%regionsprop also has the centroid function
dataAcr=regionprops(labeledAcr,'area');%regionsprop also has the centroid function
dataCor=regionprops(labeledCor,'area');

%area1=[data1.Area]';% this is now the vector of areas
%=> NOTE the division: if the pixelsize ~=1mm, then we need it; see line 51
areaPor=[dataPor.Area]'./10;%because we regridded to 5000, so a pixel=1mm
areaFav=[dataFav.Area]'./10;%make sure you get that division right!
areaPla=[dataPla.Area]'./10;
areaCyp=[dataCyp.Area]'./10;
areaAcr=[dataAcr.Area]'./10;
areaCor=[dataCor.Area]'./10;

APo=[APo;areaPor];
AFa=[AFa;areaFav];
APl=[APl;areaPla];
ACy=[ACy;areaCyp];
AAc=[AAc;areaAcr];
ACor=[ACor;areaCor];

%to calculate cover, the originally white area must be restored - because
%the PTS are polygons, they are underlain by pink rectangle, so can't use
%dimensions of pic to calculate
Bare=intersect(find(pic(:,:,1)==255), intersect(find(pic(:,:,2)==255), find(pic(:,:,3)==255)));
cov=length(Coral)./(length(Bare)+length(Coral));
cover=[cover cov];

if k < length(n)
      waitbar(k./length(n),h,n(k+1).name)
   end
end
close(h)%get rid of waitbar

% NOW AFTER THE LOOP determine the sizes classed based on percentiles of
% all corals in all transects in 2008, for 2013 and 2018 type in manually
% because the size class boundaries must remain fixed!
%we cut the largest off (eliminate outliers >90 prctile), so the size classes are:

claPor=[0:prctile(APo,90)./4:prctile(APo,90)];
claFav=[0:prctile(AFa,90)./4:prctile(AFa,90)];
claPla=[0:prctile(APl,90)./4:prctile(APl,90)];
claCyp=[0:prctile(ACy,90)./4:prctile(ACy,90)];
claAcr=[0:prctile(AAc,90)./4:prctile(AAc,90)];
claCor=[0:prctile(ACor,90)./4:prctile(ACor,90)];

%just in case that a species is lacking in an image. Else gives error.
if isnan(claCyp)
    claCyp=[0,0,0,0,0];
end
if isnan(claAcr)
    claAcr=[0,0,0,0,0];
end
if isnan(claPla)
    claPla=[0,0,0,0,0];
end
%Now force into size classes. No growing vector here, because we
%have already collected all sizes from all transects in in APo,AAc, etc. If
%done for each transect separately, then we need to introduce a growing vector here!!
%for Porites; 
%SC_Por=[length(find(APo<claPor(2))),length(find(APo>claPor(2) & APo<claPor(3))),length(find(APo>claPor(3) & APo<claPor(4))),length(find(APo>claPor(4) & APo<claPor(5))),length(find(APo>claPor(5)))];
%for Dispastrea
SC_Fav=[length(find(AFa<claFav(2))),length(find(AFa>claFav(2) & AFa<claFav(3))),length(find(AFa>claFav(3) & AFa<claFav(4))),length(find(AFa>claFav(4) & AFa<claFav(5))),length(find(AFa>claFav(5)))];
%for Platygyra 
SC_Pla=[length(find(APl<claPla(2))),length(find(APl>claPla(2) & APl<claPla(3))),length(find(APl>claPla(3) & APl<claPla(4))),length(find(APl>claPla(4) & APl<claPla(5))),length(find(APl>claPla(5)))];
%for Cyphastera
SC_Cyp=[length(find(ACy<claCyp(2))),length(find(ACy>claCyp(2) & ACy<claCyp(3))),length(find(ACy>claCyp(3) & ACy<claCyp(4))),length(find(ACy>claCyp(4) & ACy<claCyp(5))),length(find(ACy>claCyp(5)))];
%for Acropora in GCB paper #2
SC_Acr=[length(find(AAc<claAcr(2))),length(find(AAc>claAcr(2) & AAc<claAcr(3))),length(find(AAc>claAcr(3) & AAc<claAcr(4))),length(find(AAc>claAcr(4) & AAc<claAcr(5))),length(find(AAc>claAcr(5)))];
%for all corals
SC_Cor=[length(find(ACor<claCor(2))),length(find(ACor>claCor(2) & ACor<claCor(3))),length(find(ACor>claCor(3) & ACor<claCor(4))),length(find(ACor>claCor(4) & ACor<claCor(5))),length(find(ACor>claCor(5)))];

%displayed are the final data - as is done here, no further manipulation
%needed
%disp([claPor,SC_Por])
disp([claFav, SC_Fav])
disp([claPla, SC_Pla])
disp([claCyp, SC_Cyp])
disp([claAcr, SC_Acr])
disp(mean(cover))

