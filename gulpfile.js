var gulp = require('gulp');
var elm  = require('gulp-elm');

gulp.task('elm-init', elm.init);

var paths = {
  dest: 'dist',
  elm: 'src/*.elm'
};

gulp.task('elm', ['elm-init'], function () {
  return gulp.src(paths.elm)
    .pipe(elm({filetype: "html"}))
    .pipe(gulp.dest(paths.dest));
});

gulp.task('watch', function () {
  return gulp.watch(paths.elm, ['elm'])
});

gulp.task('build', ['elm'])
gulp.task('dev', ['build', 'watch'])
gulp.task('default', ['build'])
