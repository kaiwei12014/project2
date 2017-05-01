from datetime import datetime, date, time


# Custom features
# TODO: Sliding window
# TODO: Strip out data from timestamp
def extract_timestamp_data(timestamps):
	hour = timestamps.map(lambda x: (datetime.strptime(x, "%Y-%m-%d %H:%M:%S")).hour)
	weekday = timestamps.map(lambda x: (datetime.strptime(x, "%Y-%m-%d %H:%M:%S")).weekday())
	month = timestamps.map(lambda x: (datetime.strptime(x, "%Y-%m-%d %H:%M:%S")).month)
	return hour, weekday, month

# Feature transformations
# TODO: PCA/MDS

# Standardization
# TODO: Fix Missing Values
# TODO: Normal transformation

# Feature Selection
# TODO: Using Decision Tree or L1 Regression