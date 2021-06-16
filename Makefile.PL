use 5.008004;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Search::Indexer',
    VERSION_FROM      => 'lib/Search/Indexer.pm',
    PREREQ_PM         => {'Search::QueryParser'            => '0.91',
			  'BerkeleyDB'                     => '0.22',
                          'List::Util'                     => 0,
                          'List::MoreUtils'                => 0,
                          'Text::Transliterator::Unaccent' => 0,
                        },
    ABSTRACT_FROM  => 'lib/Search/Indexer.pm',
    AUTHOR         => 'Laurent Dami <dami@cpan.org>',
    META_MERGE => {
      resources => {
        homepage   => 'https://metacpan.org/author/DAMI',
        license    => 'http://www.perlfoundation.org/artistic_license_2_0',
        repository => 'https://github.com/damil/Search-Indexer.git',
      },
   },

);
