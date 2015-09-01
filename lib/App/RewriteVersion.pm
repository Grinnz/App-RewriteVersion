package App::RewriteVersion;

use strict;
use warnings;
use Carp 'croak';
use Path::Iterator::Rule;
use Path::Tiny;
use Version::Next 'next_version';

our $VERSION = '0.001';

sub new { bless {}, shift }

sub allow_decimal_underscore { shift->_boolean('allow_decimal_underscore', 0, @_) }

sub dry_run { shift->_boolean('dry_run', 0, @_) }

sub follow_symlinks { shift->_boolean('follow_symlinks', 0, @_) }

sub global { shift->_boolean('global', 0, @_) }

sub verbose { shift->_boolean('verbose', 0, @_) }

sub _boolean {
	my ($self, $attr, $default) = (shift, shift, shift);
	return $self->{$attr} //= $default unless @_;
	$self->{$attr} = shift ? 1 : 0;
	return $self;
}

sub bump_version {
	my $self = shift;
	my $version = shift // croak qq{Version is required for bump_version};
	my $bump = shift;
	
	$self->_check_version($version);
	
	if (defined $bump) {
		croak qq{Invalid bump coderef for bump_version} unless ref $bump eq 'CODE';
		$version = $bump->($version);
	} else {
		$version = next_version($version);
	}
	
	return $version;
}

sub current_version {
	my $self = shift;
	my %params = @_;
	my $dir = path($params{dir} // '.');
	my $version_from = $params{file};
	
	return $ENV{V} if exists $ENV{V};
	
	$version_from //= $self->_main_module($dir);
	return $self->version_from($version_from)
		// croak qq{No version found in file "$version_from"};
}

sub rewrite_version {
	my $self = shift;
	my $file = path(shift // croak qq{File to rewrite must be specified for rewrite_version});
	my $version = shift // croak qq{Version to rewrite must be specified for rewrite_version};
	my %params = @_;
	my $is_trial = $params{is_trial};
	
	$self->_check_version($version);
	
	return 0 unless -T $file;
	my $content = $file->slurp_utf8;
	
	my $code = qq{our \$VERSION = '$version';};
	$code .= " # TRIAL" if $is_trial;
	
	$code .= qq{\n\$VERSION = eval \$VERSION;}
		if $version =~ /_/ and scalar($version =~ /\./g) <= 1;
	
	my $assign_regex = _assign_re();
	if ($self->global ? ($content =~ s{^$assign_regex[^\n]*$}{$code}msg)
	                  : ($content =~ s{^$assign_regex[^\n]*$}{$code}ms)) {
		$file->append_utf8({truncate => 1}, $content) unless $self->dry_run;
		return 1;
	}
	
	return 0;
}

sub rewrite_versions {
	my $self = shift;
	my $version = shift // croak qq{Version to rewrite must be specified for rewrite_versions};
	my %params = @_;
	my $dir = path($params{dir} // '.');
	my $is_trial = $params{is_trial};
	
	$self->_check_version($version);
	
	my @perl_file_rules = (
		Path::Iterator::Rule->new->perl_module,
		Path::Iterator::Rule->new->perl_script,
		Path::Iterator::Rule->new->perl_test,
	);
	my $rule = Path::Iterator::Rule->new->file->ascii->skip_vcs->or(@perl_file_rules);
	my %options = (follow_symlinks => $self->follow_symlinks);
	my $iter = $rule->iter("$dir", \%options);
	while (defined(my $file = $iter->())) {
		my $rewritten = $self->rewrite_version($file, $version, is_trial => $is_trial);
		if ($self->verbose) {
			print $rewritten ? qq{Updated \$VERSION assignment in "$file" to $version\n}
				: qq{Skipping: no "our \$VERSION = '...'" found in "$file"\n};
		}
	}
	
	return $self;
}

sub version_from {
	my $self = shift;
	my $file = path(shift // croak qq{File is required for version_from});
	
	return undef unless -T $file;
	my $content = $file->slurp_utf8;
	
	my $assign_regex = _assign_re();
	my ($quote, $version) = $content =~ m{^$assign_regex[^\n]*$}ms;
	
	print qq{Extracted version from $file: $version\n} if $version and $self->verbose;
	return $version;
}

sub _check_version {
	my ($self, $version) = @_;
	croak qq{$version is not an allowed version string} unless
		$self->allow_decimal_underscore ? _is_loose_version($version) : _is_strict_version($version);
	return $self;
}

sub _dist_name {
	my ($self, $path) = @_;
	
	# Adapted from Dist::Zilla::Plugin::NameFromDirectory
	my $name = $path->absolute->basename;
	$name =~ s/(?:^(?:perl|p5)-|[\-\.]pm$)//;
	print qq{Guessing distribution name is $name\n} if $self->verbose;
	
	return $name;
}

sub _main_module {
	my ($self, $path) = @_;
	
	# Adapted from Dist::Zilla
	my $main;
	(my $guess = $self->_dist_name($path)) =~ s{-}{/}g;
	$main = $path->child("lib/$guess.pm");
	unless ($main->exists) {
		$main = path($self->_shortest_module($path));
	}
	croak qq{Could not find any modules to retrieve version from}
		unless defined $main and $main->exists;
	
	print qq{Using "$main" as dist's main module\n} if $self->verbose;
	return $main;
}

sub _shortest_module {
	my ($self, $path) = @_;
	$path = $path->child('lib');
	my $rule = Path::Iterator::Rule->new->file->ascii->skip_vcs->perl_module;
	my %options = (follow_symlinks => $self->follow_symlinks);
	return (sort { length $a <=> length $b } $rule->all($path, \%options))[0];
}

# this section copied from Dist::Zilla::Plugin::BumpVersionAfterRelease::_Util
{

# version regexes from version.pm
my $FRACTION_PART              = qr/\.[0-9]+/;
my $STRICT_INTEGER_PART        = qr/0|[1-9][0-9]*/;
my $LAX_INTEGER_PART           = qr/[0-9]+/;
my $STRICT_DOTTED_DECIMAL_PART = qr/\.[0-9]{1,3}/;
my $LAX_DOTTED_DECIMAL_PART    = qr/\.[0-9]+/;
my $LAX_ALPHA_PART             = qr/_[0-9]+/;
my $STRICT_DECIMAL_VERSION     = qr/ $STRICT_INTEGER_PART $FRACTION_PART? /x;
my $STRICT_DOTTED_DECIMAL_VERSION =
  qr/ v $STRICT_INTEGER_PART $STRICT_DOTTED_DECIMAL_PART{2,} /x;
my $STRICT = qr/ $STRICT_DECIMAL_VERSION | $STRICT_DOTTED_DECIMAL_VERSION /x;
my $LAX_DECIMAL_VERSION =
  qr/ $LAX_INTEGER_PART (?: \. | $FRACTION_PART $LAX_ALPHA_PART? )?
    |
    $FRACTION_PART $LAX_ALPHA_PART?
    /x;
my $LAX_DOTTED_DECIMAL_VERSION = qr/
    v $LAX_INTEGER_PART (?: $LAX_DOTTED_DECIMAL_PART+ $LAX_ALPHA_PART? )?
    |
    $LAX_INTEGER_PART? $LAX_DOTTED_DECIMAL_PART{2,} $LAX_ALPHA_PART?
    /x;

sub _is_strict_version { defined $_[0] && $_[0] =~ qr/\A $STRICT \z /x }

sub _is_loose_version {
    defined $_[0] && $_[0] =~ qr/\A $STRICT | $LAX_DECIMAL_VERSION \z /x;
}

# Because this is used for *capturing* or *replacing*, we take anything
# that is a lax version (but not literal string 'undef', so we don't want
# version::LAX).  Later anything captured needs to be checked with the
# strict or loose version check functions.
sub _assign_re {
    return qr{
        our \s+ \$VERSION \s* = \s*
        (['"])($LAX_DECIMAL_VERSION | $LAX_DOTTED_DECIMAL_VERSION)\1 \s* ;
        (?:\s* \# \s TRIAL)? [^\n]*
        (?:\n \$VERSION \s = \s eval \s \$VERSION;)?
    }x;
}

}
# end of copied section

1;

=head1 NAME

App::RewriteVersion - A tool to update your Perl module versions

=head1 SYNOPSIS

 use App::RewriteVersion;
 my $app = App::RewriteVersion->new;
 
 # Options
 $app->verbose(1)->follow_symlinks(0);
 
 # Bump versions for modules in current dist directory
 $app->rewrite_versions($app->bump_version($app->current_version));
 
 # Bump versions in specified dist directory
 $app->rewrite_versions($app->bump_version($app->current_version(dir => $dir)), dir => $dir);
 
 # Override module to read version from
 $app->rewrite_versions($app->bump_version($app->current_version(file => $file)));
 
 # Custom version bump algorithm
 $app->rewrite_versions($app->bump_version($app->current_version, sub { shift + 0.05 }));
 
 # Don't bump, just synchronize versions with main module
 $app->rewrite_versions($app->current_version);
 
 # Set versions to specified version
 $app->rewrite_versions('0.065');
 
=head1 DESCRIPTION

L<App::RewriteVersion> is a tool for managing Perl module versions in a
distribution. It is heavily based on the L<Dist::Zilla> plugin
L<Dist::Zilla::Plugin::RewriteVersion>. Similarly to that plugin, the C<V>
environment variable can be used to override the version detected from the main
module. Existing version assignments and new versions must be parseable with
the same rules as in L<Dist::Zilla::Plugin::RewriteVersion/"DESCRIPTION">.

See L<perl-rewrite-version> and L<perl-bump-version> for details on
command-line usage.

=head1 ATTRIBUTES

=head2 allow_decimal_underscore

 my $bool = $app->allow_decimal_underscore;
 $app = $app->allow_decimal_underscore(1);

If true, decimal versions with underscores will be allowed. Defaults to false.
As with L<Dist::Zilla::Plugin::RewriteVersion>, version tuples with underscores
are never allowed.

=head2 dry_run

 my $bool = $app->dry_run;
 $app = $app->dry_run(1);

If true, the module will process files as normal but not actually modify them.
Useful with L</"verbose"> to verify expected functionality.

=head2 follow_symlinks

 my $bool = $app->follow_symlinks;
 $app = $app->follow_symlinks(1);

If true, the application will follow symlinked directories when traversing the
distribution for modules. Defaults to false.

=head2 global

 my $bool = $app->global;
 $app = $app->global(1);

If true, the application will replace all version assignments found instead of
just the first instance in each file. Defaults to false.

=head2 verbose

 my $bool = $app->verbose;
 $app = $app->verbose(1);

Enable progress messages to be printed to STDOUT. Defaults to false.

=head1 METHODS

=head2 new

 my $app = App::RewriteVersion->new;

Construct a new L<App::RewriteVersion> object.

=head2 bump_version

 my $new_version = $app->bump_version($version);
 my $new_version = $app->bump_version($version, sub { $_[0] + 1 });

Increments a version string, returning the new version string. An optional
coderef can be passed for custom version bump logic. The coderef will receive
the current version string as the first argument and is expected to return a
new version string. By default, L<Version::Next> is used.

=head2 current_version

 my $current_version = $app->current_version;
 my $current_version = $app->current_version(dir => $dir);
 my $current_version = $app->current_version(file => $file);

Returns the current version of the distribution using L</"version_from">. If no
C<file> is passed, the main module filename will be guessed from C<dir>
(defaulting to current working directory), using heuristics similar to
L<Dist::Zilla::Plugin::NameFromDirectory> and L<Dist::Zilla/"main_module">. If
the C<V> environment variable is set, it will be returned regardless of other
options.

=head2 rewrite_version

 my $bool = $app->rewrite_version($file, $version);
 my $bool = $app->rewrite_version($file, $version, is_trial => $is_trial);

Rewrites the version of the file at C<$file> to C<$version> if it has a version
assignment in the form C<our $VERSION = '...';>. Returns true if the version
was rewritten, or false if no version assignment was found. If C<is_trial> is
true, C<# TRIAL> will be appended to the version assignment line when
rewriting.

=head2 rewrite_versions

 $app = $app->rewrite_versions($version);
 $app = $app->rewrite_versions($version, dir => $dir);
 $app = $app->rewrite_versions($version, is_trial => 1);

Rewrites the versions of all perl files found in C<dir> (defaulting to current
working directory) to C<$version> using L</"rewrite_version">. If passed,
the C<is_trial> option is passed through.

=head2 version_from

 my $version = $app->version_from($path);

Attempts to read version from the file at C<$path>. Returns C<undef> if no
version assignment was found.

=head1 BUGS

Report any issues on the public bugtracker.

=head1 AUTHOR

Dan Book <dbook@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2015 by Dan Book.

This is free software, licensed under:

  The Artistic License 2.0 (GPL Compatible)

=head1 SEE ALSO

L<perl-rewrite-version>, L<perl-bump-version>,
L<Dist::Zilla::Plugin::RewriteVersion>, L<Version::Next>
