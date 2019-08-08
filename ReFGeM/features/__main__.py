from ReFGeM.features import calculate_dimensionality_by_participant

if __name__ == '__main__':
    dataset_id = "foodstudy"
    # gps_processor_case = gps_procecssor.GPSProcessor(dataset_id)
    # feature_extractor = feature_extractor.FeatureExtractor(dataset_id)
    calculate_dimensionality_by_participant(dataset_id)