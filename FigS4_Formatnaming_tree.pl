#!usr/bin/perl;
#Description#####
#Author: Luis Bezares Calderon, written on August 19th, 2016.
#Purpose:This script replaces full name and GIs for GIs in a newicktree. It first makes a 
#list of Species names and GIs and then searches them in the newick tree.
#Input data:A list with species names and GIs is needed as a reference table.This can be easily made from NCBI Summary table.
#Data location:~./SourceDataTree/ 
#Note:The newick tree has to be previously modified by exchanging \n for every :  (in order to have a single GI per line). The desired format is such like that in the file: Tree_PKD1.txt 
#The output tree is ready to be visualized in Figtree or other compatible tree analysis software.
#Publication:Bezares-Calderon et al, 2018_FigS4.

use strict;
my $gi_list=$ARGV[0];
my $treefile=$ARGV[1];
my $outfile=$ARGV[2];


if (@ARGV < 3){
   print "Usage change_nameintree_improved2.pl <gi_list> <tree_file> <outputfilename_path>\n";
   die;
}

my @name;
my @id;
my @fulln;
my ($count,$newname);

open(TREE,"<$treefile") || die "File not found";
my @lines = <TREE>;       #Storing the newick tree in an array.
close(TREE);
open(FILE,"<$gi_list");
my @newlines;
open(NEW,">$outfile");
while(<FILE>){
	if($_=~ m/(^[\d|\w\s]*)_([\d|\w|\.|\_]*)\n/){    #finding species names and GIs.
			push(@name,$1);
			push(@id,$2);
	}
}
close(FILE);
	for(my $i=0;$i <scalar(@name);$i++){
	 $fulln[$i]="$name[$i] $id[$i]";    #Making a new array with the combination of both Species names and GIs.
	 print "ID $fulln[$i]\n";                
}

	foreach my $line(@lines) {
	 $count =0;
	 foreach my $id (@id){
	   $newname=$fulln[$count];
	   $line =~ s/$id/$newname/g;      #Substitute the new merged names for every GI.
	   $count++;
	 }
	push(@newlines,$line);        #Write the lines into a new array.
    }
    foreach(@newlines){
    	$_ =~ s/\n/:/g;			#Reformat back to the single-line Newick format.
    }
    print "@newlines"; 
	print NEW "@newlines";	# Writing the array to a new user-defined file.
close(NEW);