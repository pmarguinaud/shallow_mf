#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;


my $sub = shift;

my $F90 = 'shallow_mf_load_all.F90';

my $doc = &Fxtran::fxtran (location => $F90);

my ($pu) = &f ('.//f:program-unit[./f:subroutine-stmt/f:subroutine-N/f:N/f:n/text ()="?"]', $sub, $doc);

my @call = &f ('.//f:call-stmt[./f:procedure-designator/f:named-E/f:N/f:n/text()="LOAD"]', $pu);

$pu->insertBefore (&t ("REAL :: ZTMP\n\n"), $call[0]);

for my $call (@call)
  {
    my ($arg) = &f ('(.//f:arg/f:named-E/f:N/f:n/text ())[2]', $call);
    my $name = $arg->textContent;
    my $stmt = &Fxtran::stmt ($arg);
    my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);
    $cr->parentNode->insertAfter (&t (<< "EOF"), $cr);

#ifdef USE_ACC
ZTMP = $name
!\$acc parallel
$name = ZTMP
!\$acc end parallel
#endif

EOF
  }

'FileHandle'->new (">$F90")->print ($doc->textContent);




