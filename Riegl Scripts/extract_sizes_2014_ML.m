function extract_sizes_2014

% this is a new size-class counter made for extraction of Khalifa Port
% transects
% it expands on an older version of 2010 called Size_frequency_counter.m
% 22 September 2014, Bernhard Riegl

clear all;

n = dir('UAA T1 0419 copy.tif'); %NOTE: the folder in which the images sit must be the current directory
if isempty(n)
   disp('cd to a directory that has bmps')
   return
end


h = waitbar(0,n(1).name);
Size_cl1=[];Size_cl=[];cover=[];

for k= 1:length(n)
f0=n(k).name;
ppic=imread(f0);
%
if size(ppic,1)<size(ppic,2)%if image is horizontal
pic=imresize(ppic, [(size(ppic,1)*500./size(ppic,2)),500]); %this keeps the correct length/height relationship 
else %if image is vertical
  pic=imresize(ppic, [500,(size(ppic,1)*500./size(ppic,2))]); %this keeps the correct height/length relationship 
end
clear ppic;
%turn all Porites [0,0,255] into white blobs, lose the rest
% or turn all Poc [255,0,0] into white blobs; for that you find the color
% and index that into a black layer
Acanthastrea =          intersect(find(pic(:,:,1)==255), intersect(find(pic(:,:,2)==100), find(pic(:,:,3)==100)));
Acropora =              intersect(find(pic(:,:,1)==90), intersect(find(pic(:,:,2)==50), find(pic(:,:,3)==0)));
Anomastrea =            intersect(find(pic(:,:,1)==150), intersect(find(pic(:,:,2)==50), find(pic(:,:,3)==50)));
Coscinarea =            intersect(find(pic(:,:,1)==150), intersect(find(pic(:,:,2)==150), find(pic(:,:,3)==25)));
Cyphastrea =            intersect(find(pic(:,:,1)==10), intersect(find(pic(:,:,2)==150), find(pic(:,:,3)==138)));
Dipsastrea =            intersect(find(pic(:,:,1)==255), intersect(find(pic(:,:,2)==0), find(pic(:,:,3)==0)));
Favites =               intersect(find(pic(:,:,1)==0), intersect(find(pic(:,:,2)==0), find(pic(:,:,3)==255)));
Goniopora =             intersect(find(pic(:,:,1)==70), intersect(find(pic(:,:,2)==10), find(pic(:,:,3)==150)));
Leptastrea_purpurea =   intersect(find(pic(:,:,1)==155), intersect(find(pic(:,:,2)==0), find(pic(:,:,3)==250)));
Leptastrea_transversa = intersect(find(pic(:,:,1)==230), intersect(find(pic(:,:,2)==165), find(pic(:,:,3)==250)));
Pavona =                intersect(find(pic(:,:,1)==255), intersect(find(pic(:,:,2)==100), find(pic(:,:,3)==0)));
Platygyra =             intersect(find(pic(:,:,1)==50), intersect(find(pic(:,:,2)==255), find(pic(:,:,3)==50)));
Plesiastrea =           intersect(find(pic(:,:,1)==200), intersect(find(pic(:,:,2)==20), find(pic(:,:,3)==200)));
Porites_harrisoni =     intersect(find(pic(:,:,1)==255), intersect(find(pic(:,:,2)==255), find(pic(:,:,3)==0)));
Porites_lutea =         intersect(find(pic(:,:,1)==200), intersect(find(pic(:,:,2)==200), find(pic(:,:,3)==50)));
Psammocora_albopicta =  intersect(find(pic(:,:,1)==0), intersect(find(pic(:,:,2)==200), find(pic(:,:,3)==255)));
Psammocora_profundacella=intersect(find(pic(:,:,1)==250), intersect(find(pic(:,:,2)==150), find(pic(:,:,3)==100)));
Psammocora_stellata=    intersect(find(pic(:,:,1)==50), intersect(find(pic(:,:,2)==150), find(pic(:,:,3)==0)));
Siderastrea_savignyana =intersect(find(pic(:,:,1)==50), intersect(find(pic(:,:,2)==150), find(pic(:,:,3)==0)));
Turbinaria_peltata =    intersect(find(pic(:,:,1)==0), intersect(find(pic(:,:,2)==50), find(pic(:,:,3)==0)));
Turbinaria_reniformis = intersect(find(pic(:,:,1)==0), intersect(find(pic(:,:,2)==0), find(pic(:,:,3)==255)));
Unknown =                intersect(find(pic(:,:,1)==25), intersect(find(pic(:,:,2)==0), find(pic(:,:,3)==175)));


%!!!!!!!!!NOTE: This is done, when the sizes for ALL species are wanted in
%one analysis. To analyze each species, change Cor 1...5 to Cor and % the
%following line!
Cor=[Acanthastrea; Acropora; Anomastrea; Coscinarea; Cyphastrea; Dipsastrea; Favites; Goniopora; Favites; Goniopora; Leptastrea_purpurea; Leptastrea_transversa; Pavona; Platygyra; Plesiastrea; Porites_harrisoni; Porites_lutea; Psammocora_albopicta; Psammocora_profundacella; Psammocora_stellata; Siderastrea_savignyana; Turbinaria_peltata; Turbinaria_reniformis; Unknown];


l_b=zeros(size(pic(:,:,1)));%creates a matrix the size of pic with all zeroes = black,
l_b(Cor)=255; %changes the indices for Cor to white
Coral=l_b;
%Coral=cmedfilt2(Coral,[10,10]);%filter to reduce salt and pepper effect

% now count the areas for size/frequency
[labeled1, num1] = bwlabel(Coral,8);%bwlabel labels all the spots in a 4 or 8 neighborhood returns in NUM the number of connected objects found in BW.
[labeled2, num2] = bwlabel(Coral,4);

% convert binary image to label matrix one can also do: double(bw)
data1=regionprops(labeled1,'area');%calculates the area of white blobs. regionsprop also has the centroid function
area1=[data1.Area]'; %this is now the vector of areas
ar1=sort(area1); %in ascending order

% these are the size-graphs for the individual images
hold on;plot(ar1,'bo');set(gca,'xscale','log');set(gca,'yscale','log');xlabel('sequence');ylabel('frequency N pixels');
figure;hist(ar1);
figure;hist(sqrt(ar1./pi),4);%this expresses the area as "radius of circle equivalent"


% Force them into size classes
rad=sqrt(ar1./pi);
Sc1=[length(find(rad<5)),length(find(rad>5 & rad<10)),length(find(rad>10 & rad<30)),length(find(rad>30 & rad<100)),length(find(rad>100))];
figure; bar(Sc1);
Size_cl=[Size_cl Sc1]
% this is just to have a graphical output within the loop
%Size_cl=[Size_cl; length(find(rad<5)),length(find(rad>5 & rad<10)),length(find(rad>10 & rad<30)),length(find(rad>30 & rad<100)),length(find(rad>100))];


%to calculate cover, the originally white area must be restored
Bare=intersect(find(pic(:,:,1)==255), intersect(find(pic(:,:,2)==255), find(pic(:,:,3)==255)));
cov=length(Cor)./(length(Bare)+length(Cor));
cover=[cover cov];

if k < length(n)
      waitbar(k./length(n),h,n(k+1).name)
   end
end
close(h)

bar(sum(Size_cl)./sum(sum(Size_cl)),'facecolor',[1 100/255 100/255]); %<=== Acanthastrea
bar(sum(Size_cl)./sum(sum(Size_cl)),'facecolor',[90/255 50/255 0]);   %<=== Acropora
bar(sum(Size_cl)./sum(sum(Size_cl)),'facecolor',[150/255 50/255 50/255]);

bar(sum(Size_cl)./sum(sum(Size_cl)),'facecolor',[0 0 1]);             %<===
bar(sum(Size_cl)./sum(sum(Size_cl)),'facecolor',[50/255 1 50/255]);
%bar(sum(Size_cl)./sum(sum(Size_cl)),'facecolor',[1 127/255 0]);
%bar(sum(Size_cl)./sum(sum(Size_cl)),'facecolor',[100/255 100/255 100/255]);

explode=[1,1];cmap = [255 255 255; 255 100 100]; cmap=cmap./255;
explode=[1,1];cmap = [255 255 255; 0 0 255]; cmap=cmap./255;
explode=[1,1];cmap = [255 255 255; 50 255 50]; cmap=cmap./255;
%explode=[1,1];cmap = [255 255 255; 255 127 0]; cmap=cmap./255;
%explode=[1,1];cmap = [255 255 255; 100 100 100]; cmap=cmap./255;

figure; pie([1-mean(cover), mean(cover)],explode);colormap(cmap); 

% 
% %note these are the mean radii

%Distr=sum(Size_cl)./sum(sum(Size_cl))
%mean(cover)*100
% 
% figure;bar(m_h,'facecolor','k');axis([0.5 4.5 0 1]); figure;bar(m_s,'facecolor','k');axis([0.5 4.5 0 1]);
% 
stats=[mean(Cov); std(Cov)]
