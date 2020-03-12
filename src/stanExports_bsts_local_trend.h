// Generated by rstantools.  Do not edit by hand.

/*
    epipredictr is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    epipredictr is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with epipredictr.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.19.1
#include <stan/model/model_header.hpp>
namespace model_bsts_local_trend_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_bsts_local_trend");
    reader.add_event(102, 100, "end", "model_bsts_local_trend");
    return reader;
}
#include <stan_meta_header.hpp>
class model_bsts_local_trend : public prob_grad {
private:
        int N;
        std::vector<double> y;
        int n_pred;
        double prior_var_phi;
        int length_local_trend;
        double mean_phi;
        std::vector<double> r;
        vector_d x;
public:
    model_bsts_local_trend(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_bsts_local_trend(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_bsts_local_trend_namespace::model_bsts_local_trend";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 2;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            current_statement_begin__ = 3;
            validate_non_negative_index("y", "N", N);
            context__.validate_dims("data initialization", "y", "double", context__.to_vec(N));
            y = std::vector<double>(N, double(0));
            vals_r__ = context__.vals_r("y");
            pos__ = 0;
            size_t y_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < y_k_0_max__; ++k_0__) {
                y[k_0__] = vals_r__[pos__++];
            }
            current_statement_begin__ = 4;
            context__.validate_dims("data initialization", "n_pred", "int", context__.to_vec());
            n_pred = int(0);
            vals_i__ = context__.vals_i("n_pred");
            pos__ = 0;
            n_pred = vals_i__[pos__++];
            current_statement_begin__ = 5;
            context__.validate_dims("data initialization", "prior_var_phi", "double", context__.to_vec());
            prior_var_phi = double(0);
            vals_r__ = context__.vals_r("prior_var_phi");
            pos__ = 0;
            prior_var_phi = vals_r__[pos__++];
            current_statement_begin__ = 6;
            context__.validate_dims("data initialization", "length_local_trend", "int", context__.to_vec());
            length_local_trend = int(0);
            vals_i__ = context__.vals_i("length_local_trend");
            pos__ = 0;
            length_local_trend = vals_i__[pos__++];
            check_greater_or_equal(function__, "length_local_trend", length_local_trend, 1);
            current_statement_begin__ = 7;
            context__.validate_dims("data initialization", "mean_phi", "double", context__.to_vec());
            mean_phi = double(0);
            vals_r__ = context__.vals_r("mean_phi");
            pos__ = 0;
            mean_phi = vals_r__[pos__++];
            // initialize transformed data variables
            current_statement_begin__ = 11;
            validate_non_negative_index("r", "N", N);
            r = std::vector<double>(N, double(0));
            stan::math::fill(r, DUMMY_VAR__);
            current_statement_begin__ = 12;
            validate_non_negative_index("x", "length_local_trend", length_local_trend);
            x = Eigen::Matrix<double, Eigen::Dynamic, 1>(length_local_trend);
            stan::math::fill(x, DUMMY_VAR__);
            // execute transformed data statements
            current_statement_begin__ = 13;
            for (int i = 1; i <= N; ++i) {
                current_statement_begin__ = 14;
                stan::model::assign(r, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            stan::math::log((get_base1(y, i, "y", 1) / (15 - get_base1(y, i, "y", 1)))), 
                            "assigning variable r");
            }
            current_statement_begin__ = 16;
            for (int i = 1; i <= length_local_trend; ++i) {
                current_statement_begin__ = 17;
                stan::model::assign(x, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            i, 
                            "assigning variable x");
            }
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 23;
            num_params_r__ += 1;
            current_statement_begin__ = 24;
            num_params_r__ += 1;
            current_statement_begin__ = 25;
            num_params_r__ += 1;
            current_statement_begin__ = 26;
            validate_non_negative_index("delta", "N", N);
            num_params_r__ += N;
            current_statement_begin__ = 27;
            validate_non_negative_index("D", "N", N);
            num_params_r__ += N;
            current_statement_begin__ = 28;
            validate_non_negative_index("intercept", "N", N);
            num_params_r__ += N;
            current_statement_begin__ = 29;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_bsts_local_trend() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 23;
        if (!(context__.contains_r("sigma_epsilon")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma_epsilon missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma_epsilon");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma_epsilon", "double", context__.to_vec());
        double sigma_epsilon(0);
        sigma_epsilon = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sigma_epsilon);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma_epsilon: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 24;
        if (!(context__.contains_r("sigma_eta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma_eta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma_eta");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma_eta", "double", context__.to_vec());
        double sigma_eta(0);
        sigma_eta = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sigma_eta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma_eta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 25;
        if (!(context__.contains_r("phi")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable phi missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("phi");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "phi", "double", context__.to_vec());
        double phi(0);
        phi = vals_r__[pos__++];
        try {
            writer__.scalar_lub_unconstrain(-(1), 1, phi);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable phi: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 26;
        if (!(context__.contains_r("delta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable delta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("delta");
        pos__ = 0U;
        validate_non_negative_index("delta", "N", N);
        context__.validate_dims("parameter initialization", "delta", "vector_d", context__.to_vec(N));
        Eigen::Matrix<double, Eigen::Dynamic, 1> delta(N);
        size_t delta_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < delta_j_1_max__; ++j_1__) {
            delta(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(delta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable delta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 27;
        if (!(context__.contains_r("D")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable D missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("D");
        pos__ = 0U;
        validate_non_negative_index("D", "N", N);
        context__.validate_dims("parameter initialization", "D", "vector_d", context__.to_vec(N));
        Eigen::Matrix<double, Eigen::Dynamic, 1> D(N);
        size_t D_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < D_j_1_max__; ++j_1__) {
            D(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(D);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable D: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 28;
        if (!(context__.contains_r("intercept")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable intercept missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("intercept");
        pos__ = 0U;
        validate_non_negative_index("intercept", "N", N);
        context__.validate_dims("parameter initialization", "intercept", "vector_d", context__.to_vec(N));
        Eigen::Matrix<double, Eigen::Dynamic, 1> intercept(N);
        size_t intercept_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < intercept_j_1_max__; ++j_1__) {
            intercept(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(intercept);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable intercept: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 29;
        if (!(context__.contains_r("sd_trend")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sd_trend missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sd_trend");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sd_trend", "double", context__.to_vec());
        double sd_trend(0);
        sd_trend = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sd_trend);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sd_trend: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 23;
            local_scalar_t__ sigma_epsilon;
            (void) sigma_epsilon;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma_epsilon = in__.scalar_lb_constrain(0, lp__);
            else
                sigma_epsilon = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 24;
            local_scalar_t__ sigma_eta;
            (void) sigma_eta;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma_eta = in__.scalar_lb_constrain(0, lp__);
            else
                sigma_eta = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 25;
            local_scalar_t__ phi;
            (void) phi;  // dummy to suppress unused var warning
            if (jacobian__)
                phi = in__.scalar_lub_constrain(-(1), 1, lp__);
            else
                phi = in__.scalar_lub_constrain(-(1), 1);
            current_statement_begin__ = 26;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> delta;
            (void) delta;  // dummy to suppress unused var warning
            if (jacobian__)
                delta = in__.vector_constrain(N, lp__);
            else
                delta = in__.vector_constrain(N);
            current_statement_begin__ = 27;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> D;
            (void) D;  // dummy to suppress unused var warning
            if (jacobian__)
                D = in__.vector_constrain(N, lp__);
            else
                D = in__.vector_constrain(N);
            current_statement_begin__ = 28;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> intercept;
            (void) intercept;  // dummy to suppress unused var warning
            if (jacobian__)
                intercept = in__.vector_constrain(N, lp__);
            else
                intercept = in__.vector_constrain(N);
            current_statement_begin__ = 29;
            local_scalar_t__ sd_trend;
            (void) sd_trend;  // dummy to suppress unused var warning
            if (jacobian__)
                sd_trend = in__.scalar_lb_constrain(0, lp__);
            else
                sd_trend = in__.scalar_lb_constrain(0);
            // model body
            current_statement_begin__ = 37;
            for (int s = 1; s <= (N - 1); ++s) {
                current_statement_begin__ = 40;
                lp_accum__.add(normal_log<propto__>(get_base1(y, (s + 1), "y", 1), (get_base1(y, s, "y", 1) + get_base1(delta, s, "delta", 1)), sigma_epsilon));
                current_statement_begin__ = 41;
                lp_accum__.add(normal_log<propto__>(get_base1(delta, (s + 1), "delta", 1), (get_base1(D, s, "D", 1) + (phi * (get_base1(delta, s, "delta", 1) - get_base1(D, s, "D", 1)))), sigma_eta));
            }
            current_statement_begin__ = 43;
            lp_accum__.add(normal_log<propto__>(get_base1(delta, 1, "delta", 1), D, sigma_eta));
            current_statement_begin__ = 50;
            lp_accum__.add(normal_log<propto__>(D, 0, 1));
            current_statement_begin__ = 51;
            lp_accum__.add(normal_log<propto__>(intercept, 0, 1));
            current_statement_begin__ = 53;
            for (int s = length_local_trend; s <= N; ++s) {
                current_statement_begin__ = 54;
                lp_accum__.add(normal_log<propto__>(stan::model::rvalue(y, stan::model::cons_list(stan::model::index_min_max(((s - length_local_trend) + 1), s), stan::model::nil_index_list()), "y"), add(get_base1(intercept, s, "intercept", 1), multiply(x, get_base1(D, s, "D", 1))), sd_trend));
            }
            current_statement_begin__ = 57;
            lp_accum__.add(normal_log<propto__>(sd_trend, 0, 2));
            current_statement_begin__ = 58;
            lp_accum__.add(normal_log<propto__>(intercept, 0, 3));
            current_statement_begin__ = 60;
            lp_accum__.add(normal_log<propto__>(phi, mean_phi, prior_var_phi));
            current_statement_begin__ = 61;
            lp_accum__.add(exponential_log<propto__>(sigma_eta, 3));
            current_statement_begin__ = 62;
            lp_accum__.add(exponential_log<propto__>(sigma_epsilon, 3));
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("sigma_epsilon");
        names__.push_back("sigma_eta");
        names__.push_back("phi");
        names__.push_back("delta");
        names__.push_back("D");
        names__.push_back("intercept");
        names__.push_back("sd_trend");
        names__.push_back("y_pred");
        names__.push_back("delta_pred");
        names__.push_back("y_post");
        names__.push_back("phi_prior");
        names__.push_back("sigma_eta_prior");
        names__.push_back("sigma_epsilon_prior");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(N);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(N);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(N);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(n_pred);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(n_pred);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(N);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_bsts_local_trend_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        double sigma_epsilon = in__.scalar_lb_constrain(0);
        vars__.push_back(sigma_epsilon);
        double sigma_eta = in__.scalar_lb_constrain(0);
        vars__.push_back(sigma_eta);
        double phi = in__.scalar_lub_constrain(-(1), 1);
        vars__.push_back(phi);
        Eigen::Matrix<double, Eigen::Dynamic, 1> delta = in__.vector_constrain(N);
        size_t delta_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < delta_j_1_max__; ++j_1__) {
            vars__.push_back(delta(j_1__));
        }
        Eigen::Matrix<double, Eigen::Dynamic, 1> D = in__.vector_constrain(N);
        size_t D_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < D_j_1_max__; ++j_1__) {
            vars__.push_back(D(j_1__));
        }
        Eigen::Matrix<double, Eigen::Dynamic, 1> intercept = in__.vector_constrain(N);
        size_t intercept_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < intercept_j_1_max__; ++j_1__) {
            vars__.push_back(intercept(j_1__));
        }
        double sd_trend = in__.scalar_lb_constrain(0);
        vars__.push_back(sd_trend);
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
            // declare and define generated quantities
            current_statement_begin__ = 68;
            validate_non_negative_index("y_pred", "n_pred", n_pred);
            std::vector<double> y_pred(n_pred, double(0));
            stan::math::initialize(y_pred, DUMMY_VAR__);
            stan::math::fill(y_pred, DUMMY_VAR__);
            current_statement_begin__ = 69;
            validate_non_negative_index("delta_pred", "n_pred", n_pred);
            std::vector<double> delta_pred(n_pred, double(0));
            stan::math::initialize(delta_pred, DUMMY_VAR__);
            stan::math::fill(delta_pred, DUMMY_VAR__);
            current_statement_begin__ = 70;
            validate_non_negative_index("y_post", "N", N);
            std::vector<double> y_post(N, double(0));
            stan::math::initialize(y_post, DUMMY_VAR__);
            stan::math::fill(y_post, DUMMY_VAR__);
            current_statement_begin__ = 74;
            double phi_prior;
            (void) phi_prior;  // dummy to suppress unused var warning
            stan::math::initialize(phi_prior, DUMMY_VAR__);
            stan::math::fill(phi_prior, DUMMY_VAR__);
            current_statement_begin__ = 75;
            double sigma_eta_prior;
            (void) sigma_eta_prior;  // dummy to suppress unused var warning
            stan::math::initialize(sigma_eta_prior, DUMMY_VAR__);
            stan::math::fill(sigma_eta_prior, DUMMY_VAR__);
            current_statement_begin__ = 76;
            double sigma_epsilon_prior;
            (void) sigma_epsilon_prior;  // dummy to suppress unused var warning
            stan::math::initialize(sigma_epsilon_prior, DUMMY_VAR__);
            stan::math::fill(sigma_epsilon_prior, DUMMY_VAR__);
            // generated quantities statements
            current_statement_begin__ = 80;
            stan::math::assign(phi_prior, normal_rng(mean_phi, prior_var_phi, base_rng__));
            current_statement_begin__ = 81;
            stan::math::assign(sigma_eta_prior, exponential_rng(3, base_rng__));
            current_statement_begin__ = 82;
            stan::math::assign(sigma_epsilon_prior, exponential_rng(3, base_rng__));
            current_statement_begin__ = 86;
            stan::model::assign(y_post, 
                        stan::model::cons_list(stan::model::index_uni(1), stan::model::nil_index_list()), 
                        normal_rng(get_base1(y, 1, "y", 1), sigma_epsilon, base_rng__), 
                        "assigning variable y_post");
            current_statement_begin__ = 87;
            for (int s = 1; s <= (N - 1); ++s) {
                current_statement_begin__ = 88;
                stan::model::assign(y_post, 
                            stan::model::cons_list(stan::model::index_uni((s + 1)), stan::model::nil_index_list()), 
                            normal_rng((get_base1(y_post, s, "y_post", 1) + get_base1(delta, s, "delta", 1)), sigma_epsilon, base_rng__), 
                            "assigning variable y_post");
            }
            current_statement_begin__ = 93;
            stan::model::assign(y_pred, 
                        stan::model::cons_list(stan::model::index_uni(1), stan::model::nil_index_list()), 
                        normal_rng((get_base1(y, N, "y", 1) + get_base1(delta, N, "delta", 1)), sigma_epsilon, base_rng__), 
                        "assigning variable y_pred");
            current_statement_begin__ = 94;
            stan::model::assign(delta_pred, 
                        stan::model::cons_list(stan::model::index_uni(1), stan::model::nil_index_list()), 
                        normal_rng((get_base1(D, N, "D", 1) + (phi * (get_base1(delta, N, "delta", 1) - get_base1(D, N, "D", 1)))), sigma_epsilon, base_rng__), 
                        "assigning variable delta_pred");
            current_statement_begin__ = 96;
            for (int s = 1; s <= (n_pred - 1); ++s) {
                current_statement_begin__ = 97;
                stan::model::assign(y_pred, 
                            stan::model::cons_list(stan::model::index_uni((s + 1)), stan::model::nil_index_list()), 
                            normal_rng((get_base1(y_pred, s, "y_pred", 1) + get_base1(delta_pred, s, "delta_pred", 1)), sigma_epsilon, base_rng__), 
                            "assigning variable y_pred");
                current_statement_begin__ = 98;
                stan::model::assign(delta_pred, 
                            stan::model::cons_list(stan::model::index_uni((s + 1)), stan::model::nil_index_list()), 
                            normal_rng((get_base1(D, N, "D", 1) + (phi * (get_base1(delta_pred, s, "delta_pred", 1) - get_base1(D, N, "D", 1)))), sigma_epsilon, base_rng__), 
                            "assigning variable delta_pred");
            }
            // validate, write generated quantities
            current_statement_begin__ = 68;
            size_t y_pred_k_0_max__ = n_pred;
            for (size_t k_0__ = 0; k_0__ < y_pred_k_0_max__; ++k_0__) {
                vars__.push_back(y_pred[k_0__]);
            }
            current_statement_begin__ = 69;
            size_t delta_pred_k_0_max__ = n_pred;
            for (size_t k_0__ = 0; k_0__ < delta_pred_k_0_max__; ++k_0__) {
                vars__.push_back(delta_pred[k_0__]);
            }
            current_statement_begin__ = 70;
            size_t y_post_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < y_post_k_0_max__; ++k_0__) {
                vars__.push_back(y_post[k_0__]);
            }
            current_statement_begin__ = 74;
            vars__.push_back(phi_prior);
            current_statement_begin__ = 75;
            vars__.push_back(sigma_eta_prior);
            current_statement_begin__ = 76;
            vars__.push_back(sigma_epsilon_prior);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    static std::string model_name() {
        return "model_bsts_local_trend";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_epsilon";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_eta";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "phi";
        param_names__.push_back(param_name_stream__.str());
        size_t delta_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < delta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "delta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t D_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < D_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "D" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t intercept_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < intercept_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "intercept" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sd_trend";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
        size_t y_pred_k_0_max__ = n_pred;
        for (size_t k_0__ = 0; k_0__ < y_pred_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "y_pred" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t delta_pred_k_0_max__ = n_pred;
        for (size_t k_0__ = 0; k_0__ < delta_pred_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "delta_pred" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t y_post_k_0_max__ = N;
        for (size_t k_0__ = 0; k_0__ < y_post_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "y_post" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "phi_prior";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_eta_prior";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_epsilon_prior";
        param_names__.push_back(param_name_stream__.str());
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_epsilon";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_eta";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "phi";
        param_names__.push_back(param_name_stream__.str());
        size_t delta_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < delta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "delta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t D_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < D_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "D" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t intercept_j_1_max__ = N;
        for (size_t j_1__ = 0; j_1__ < intercept_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "intercept" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "sd_trend";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
        size_t y_pred_k_0_max__ = n_pred;
        for (size_t k_0__ = 0; k_0__ < y_pred_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "y_pred" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t delta_pred_k_0_max__ = n_pred;
        for (size_t k_0__ = 0; k_0__ < delta_pred_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "delta_pred" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t y_post_k_0_max__ = N;
        for (size_t k_0__ = 0; k_0__ < y_post_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "y_post" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "phi_prior";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_eta_prior";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_epsilon_prior";
        param_names__.push_back(param_name_stream__.str());
    }
}; // model
}  // namespace
typedef model_bsts_local_trend_namespace::model_bsts_local_trend stan_model;
#endif
