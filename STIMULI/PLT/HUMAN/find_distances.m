clear all;close all;clc;

pict_ext='png';
dyads_folder='faway'; % faway facing
back_color=230;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
current_fold=fullfile(fileparts(mfilename('fullpath')));
stimuliPath=fullfile(current_fold,dyads_folder);
listOfJpegs = dir(fullfile(stimuliPath,['*.' pict_ext]));

numberOfJpegs = numel(listOfJpegs);
cd(stimuliPath)

distance_centers=zeros(numberOfJpegs,1);
distance_extremities=zeros(numberOfJpegs,1);
%data_centermass=zeros(numberOfJpegs,1);
%%
for k = 1:numberOfJpegs
    information1 = listOfJpegs(k);
    jpegName1 = information1.name;
    
    f =imread(jpegName1);
    RA = imref2d(size(3)); %
    
%     [r,c] = size(f)
%     X = zeros(r,c)
%     for k = 1:r
%         for j = 1:c
%             if f(k,j)==i
%                X(k,j)=1;
%             end        
%         end
%     end
% 
%     %disp(X)
% 
%     cogR=centroid(X);
%     cogC=centroid(X,2);
% 
%     disp(cogR)
%     disp(cogC)
%     
% end
    
    [a,map,alpha] = imread(jpegName1);

    % find the image limits and center the image 
    [r c z] = size(a);
    for q=1:c
%         if any(find((a(:,q,1)<back_color) | a(:,q,2)<back_color | a(:,q,3)<back_color)); %3d color images
        if any(find(a(:,q,:)<back_color)) && any(find(a(:,q+1,1)<back_color))
            q_start1=q;
            break
        end
    end
    for q=q_start1+1:c
%         if isempty(find((a(:,q,1)<back_color) | a(:,q,2)<back_color | a(:,q,3)<back_color));
        if isempty(find(a(:,q,:)<back_color)) && isempty(find(a(:,q+1,1)<back_color))% | a(:,q,2)<back_color | a(:,q,3)<back_color));
            q_end1=q;
            break
        end   
    end

    
    q_center1=q_start1+(q_end1-q_start1)/2;
%%    
    %half1=f(:,1:c/2,:); %f(:,1:c/2,:);
    %half2=f(:,c/2+1:c,:);
    %q_center1_cm=centerOfMass(half1);
%%
    
%     xtrans=round(((c/2)-q_center));
    
    
    for q=q_end1+1:c
%         if any(find((a(:,q,1)<back_color) | a(:,q,2)<back_color | a(:,q,3)<back_color));
        if any(find(a(:,q,:)<back_color)) && any(find(a(:,q+1,1)<back_color))% | a(:,q,2)<back_color | a(:,q,3)<back_color));
            q_start2=q;
            break
        end
    end
    for q=q_start2+1:c
%         if isempty(find((a(:,q,1)<back_color) | a(:,q,2)<back_color | a(:,q,3)<back_color));
        if isempty(find(a(:,q,:)<back_color)) && isempty(find(a(:,q,1)<back_color)) % | a(:,q,2)<back_color | a(:,q,3)<back_color));
            q_end2=q;
            break
        else 
            q_end2=c;
            break
        end   
    end

    q_center2=q_start2+(q_end2-q_start2)/2;
%%    
    %q_center2_cm=centerOfMass(half2);
%%  
    distance_extremities(k)=q_start2-q_end1;
    distance_centers(k)=q_center2-q_center1;

%%    
    %distance_centermass(k)=q_center2_cm(2)-q_center1_cm(2);

    
end

%% questa parte organizza dati per condizioni

numItems = numberOfJpegs; %480 posner
numCond= 1;

data_extremities = zeros(numItems, numCond); %create new array ENTER SUBJECTS NUMBER
data_centers = zeros(numItems, numCond); %create new array ENTER SUBJECTS NUMBER
%data_centermass = zeros(numItems, numCond);
kk=[1:numItems:numberOfJpegs]; 


for j= 1:numel(kk)-1

    data_extremities(1:numItems,j)=distance_extremities(kk(j):kk(j+1)-1);
    data_centers(1:numItems,j)=distance_centers(kk(j):kk(j+1)-1);
    %data_centermass(1:numItems,j)=distance_centermass(kk(j):kk(j+1)-1);
    %data(1:720,1)=a(k(j):k((j+1)-1));
    %data(1:720,1)=a(1:720)
    %data(1:720,2)=a(721:1440)
    
end

data_extremities(1:numItems,length(kk))=distance_extremities((numberOfJpegs-(numItems-1)):numberOfJpegs);

data_centers(1:numItems,length(kk))=distance_centers((numberOfJpegs-(numItems-1)):numberOfJpegs);

%data_centermass(1:numItems,length(kk))=distance_centermass((numberOfJpegs-(numItems-1)):numberOfJpegs);

%sum(find(distance_extremities(1:30)==distance_extremities(31:60)))

%     xtrans=round(((c/2)-q_center));
    

% if xtrans>=0
%     new_a=zeros(size(a));
%     for q=1:c-xtrans
%         new_a(:,q+xtrans,:)=a(:,q,:);
%     end
% elseif xtrans<0
%     new_a=zeros(size(a));
%     for q=-xtrans:c
%         new_a(:,q+xtrans,:)=a(:,q,:);
%     end 
% end
% sizefinale=[ 1264 631 3]

%     A=imtranslate(a,[xtrans,0]);
%     alphaA=imtranslate(alpha,[xtrans,0]);
% 
%     indices_image=round([c/2-sizefinale(1)/2+1, c/2+sizefinale(1)/2, 1+size(A,2)/2-sizefinale(2)/2,size(A,2)/2+sizefinale(2)/2]);
%     newA(1:sizefinale(1),1:sizefinale(2),1:sizefinale(3))=A(indices_image(1):indices_image(2),indices_image(3):indices_image(4),1:3);
% 
%     alphanewA(1:sizefinale(1),1:sizefinale(2))=alphaA(indices_image(1):indices_image(2),indices_image(3):indices_image(4));


%     I = imshow(newA,'Parent',ax2);
%     set(I,'AlphaData',alphanewA);
%     % RA = imref2d(size(newA));
%     % imcrop(I,round([c/2-sizefinale(1)/2,c/2+sizefinale(1)/2,size(A,2)/2-sizefinale(2)/2,size(A,2)/2+sizefinale(2)/2]))
% 
%     [bg,mapbg,alphabg] = imread('BGb.png');
%     % Rbg = imref2d(size(bg));
% 
%     % combo=imfuse(newA,RA,bg,Rbg,'blend');
% 
%     I2 = imshow('BGb.png','Parent',ax1);
% 
%     %set(gcf,'PaperPositionMode','auto')
%     
%     filename = [outputPath ['0_' jpegName1 '.jpg']];%[outputPath ['0' num2str(k+15) 'f.jpg']];
%     
%     saveas(figure1,filename)
% 
% 
%     %crop
%     C= imread('test4.png');
%     [r c z] = size(C);
%     for q=1:c
%         if any(find((C(:,q,1)<back_color) | C(:,q,2)<back_color | C(:,q,3)<back_color));
%             q_start=q;
%             break
%         end
%     end
%     for q=q_start+1:c
%         if isempty(find((C(:,q,1)<back_color) | C(:,q,2)<back_color | C(:,q,3)<back_color));
%             q_end=q;
%             break
%         end   
%     end
% 
%     for p=1:r
%         if any(find((C(p,:,1)<back_color) | C(p,:,2)<back_color | C(p,:,3)<back_color));
%             p_start=p;
%             break
%         end
%     end
%     for p=p_start+1:r
%         if isempty(find((C(p,:,1)<back_color) | C(p,:,2)<back_color | C(p,:,3)<back_color));
%             p_end=p;
%             break
%         end   
%     end
% 
%     newC=C(p_start+1:p_end-1,q_start+1:q_end-1,1:z); % the "+1" and "-1" are to avoid the black lines in the picture.
%     imwrite(newC,'test7.png')
% end
% next steps:
% 1. imfuse two images
% 2. crop them (perhaps not necessary)
% 3. adjust contrast
% 4. rename them (targetN) 


csvwrite(['distances_' dyads_folder '.csv'], distance_extremities)
