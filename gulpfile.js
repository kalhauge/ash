var gulp = require('gulp');
var elm  = require('gulp-elm');
var browserSync = require('browser-sync');
var plumber = require('gulp-plumber');

gulp.task('elm-init', elm.init);

var paths = {
  dest: 'dist',
  elm: 'src/*.elm'
};

gulp.task('elm', ['elm-init'], function () {
  return gulp.src(paths.elm)
    .pipe(plumber())
    .pipe(elm({filetype: "html"}))
    .pipe(gulp.dest(paths.dest));
});

gulp.task('watch', function () {
  browserSync({
    server: {
      baseDir: paths.dest,
      index: "Ash.html"
    }
  })
  gulp.watch(paths.elm, ['elm'])
  gulp.watch('dist/Ash.html').on('change', browserSync.reload);
});

gulp.task('build', ['elm'])
gulp.task('dev', ['build', 'watch'])
gulp.task('default', ['build'])
