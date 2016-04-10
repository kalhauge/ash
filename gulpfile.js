var gulp = require('gulp');
var elm  = require('gulp-elm');
var browserSync = require('browser-sync');
var plumber = require('gulp-plumber');

gulp.task('elm-init', elm.init);

var paths = {
  dest: 'dist',
  elm: 'src/*.elm',
  staticfiles: 'static/*.html'
};

gulp.task('elm', ['elm-init'], function () {
  return gulp.src(paths.elm)
    .pipe(plumber())
    .pipe(elm.bundle("bundle.js"))
    .pipe(gulp.dest(paths.dest));
});

gulp.task('static', function () {
  gulp.src(paths.staticfiles)
    .pipe(gulp.dest(paths.dest))
});

gulp.task('watch', function () {
  browserSync({
    server: {
      baseDir: paths.dest,
    }
  })
  gulp.watch(paths.elm, ['elm'])
  gulp.watch(paths.staticfiles, ['static'])
  gulp.watch('dist/*').on('change', browserSync.reload);
});

gulp.task('build', ['elm', 'static'])
gulp.task('dev', ['build', 'watch'])
gulp.task('default', ['build'])
