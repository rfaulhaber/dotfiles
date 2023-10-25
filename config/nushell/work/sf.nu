# helpers for 'sf'

export def get_bulk_failures [
       --target-org (-o): string
       job_id: string
] {
    let org_info = (get_org_info $target_org)
    let token = ($org_info.accessToken)
    let instance_url = ($org_info.instanceUrl)

    http get -H ["Authorization" $"Bearer $token" "Accept" "text/csv"] $"$instance_url/services/data/v59.0/jobs/ingest/$job_id/failedResults"
}

def get_org_info [org] {
    sf org display --json -o $org | from json | get result
}
