import numpy as np


# TODO: Test Assumptions about data...autocorrelation, normality, heteroskedactity, etc.

# TODO: Test predicted output is nonnegative and reasonable

# TODO: Test predictions using RMSLE
def rmsle(actual_values, predicted_values):
    squared_log_errors = (np.log(np.array(predicted_values) + 1) - np.log(np.array(actual_values) + 1)) ** 2
    mean_squared_errors = np.nansum(squared_log_errors) / len(squared_log_errors)
    return np.sqrt(mean_squared_errors)

# TODO: Test submission has same shape as sample submission
def check_submission_compatibility(sample_submission_df, pred_df):
	return sample_submission_df.shape == pred_df.shape