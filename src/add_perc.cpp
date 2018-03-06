#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//



//' @title add_perc_cpp
//' @description The c++ version of \code{\link{add_perc}} function.
//'   Adds percentage distribution columns to a summarised data frame
//' @param data A data frame that has a date or similar variable as its first
//'   column, followed by the levels of some categorical variable that makes up a
//'   total, which is the last column. Typically a data frame created by the
//'   \code{\link{vardistr_amnt}} function.
//'   @param start_col start start summing across from this column number
//'
//' @return the same data frame plus a column starting with "perc" for each of
//'   the existing variables. These added variables will add up to 100% if they
//'   are summed up row wise.
//' @export
//'
//' @examples
//' x <- add_perc_cpp(df, 2)
//'
// [[Rcpp::export]]
DataFrame add_perc_cpp(DataFrame df, int start_col) {

  int ncolumns = df.length(); // get number of columns in df

  CharacterVector col_names = df.attr("names"); // get df column names

  // print outs
  // Rcout << "start_col : " << start_col << "\n";
  // Rcout << "ncolumns : " << ncolumns << "\n";
  // Rcout << "col_names : " << col_names << "\n";

  NumericVector total = df["total"]; // read in the column vector 'total'

  // iterate only across variables (from second column to second last column of df)
  // exclude: first column is typically date column `pointintime_month` and last column is `total`
  //for(DataFrame::iterator i = df.begin()+1; i != df.end()-1; ++i) {
  for(int i = start_col-1; i != ncolumns-1; ++i) {

   String colname = col_names[i]; //get df column name at iteration
   NumericVector col_variable = df[colname];  // read in the column vector by name
   NumericVector percentage_val = col_variable/total; //calculate percentage value
   String perc_string("perc_");
   perc_string += colname; // append column name to "perc_"
   df.push_back(percentage_val, perc_string); // cbind percentage_val vector to df

  }
  // return a new data frame
  //return DataFrame::create(_["a"]= a, _["b"]= b);
  return df;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R

df <- data.frame(pointintime_month = c("2010-04-30", "2010-05-31", "2010-06-30", "2010-07-31"),
                 var1 = c(14573685, 32302144, 42579558, 59380592),
                 var2 = c(1399102, 6930954, 13801721, 23130840),
                 total = c(15972787, 39233098, 56381279, 82511432))

df_new <- add_perc_cpp(df, start_col = 2)


*/
