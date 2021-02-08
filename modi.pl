#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;


my $F90 = shift;
my $mod = shift;

my $doc = &Fxtran::fxtran (location => $F90);

my @pu = &f ('./f:object/f:file/f:program-unit', $doc);

for my $pu (@pu)
  {
    my $stmt = $pu->firstChild;

    (my $kind = $stmt->nodeName ()) =~ s/-stmt$//o;

    my ($name) = &f ('./f:' . $kind . '-N/f:N/f:n/text ()', $stmt, 1);
    my @args = &f ('.//f:dummy-arg-LT//f:arg-N/f:N/f:n/text ()', $stmt, 1);
    
    my %stmt;
    
    # Keep first & last statements
    
    $stmt{$pu->firstChild} = $pu->firstChild;
    $stmt{$pu->lastChild}  = $pu->lastChild;
    
    # Keep declaration statements referecing arguments
    
    for my $arg (@args)
      {
        my @en = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n[text ()="' . $arg . '"]]', $pu);
        for my $en (@en)
          {
            my $stmt = &Fxtran::stmt ($en);
            $stmt{$stmt} = $stmt;
          }
      }
    
    my @stmt = &f ('.//' . &Fxtran::xpath_by_type ('stmt'), $pu);
    
    for my $stmt (@stmt)
      {
        $stmt->unbindNode () unless ($stmt{$stmt});
      }

    unless ($mod)
      {
        $mod = lc ("modi_${name}");
      }

  }


# Strip comments

for (&f ('.//f:C', $doc))
  {
    $_->unbindNode ();
  }

# Strip empty lines

my $text = $doc->textContent ();

$text =~ s/^\s*\n$//goms;


$mod = lc ($mod);
my $MOD = uc ($mod);

'FileHandle'->new (">$mod.F90")->print (<< "EOF");
MODULE $MOD

INTERFACE
$text
END INTERFACE

END MODULE
EOF
