
export interface CodeChunk {
  id: string;
  originalName: string;
  cobolCode: string;
  pythonCode?: string;
  unitTests?: string;
  status: 'pending' | 'analyzing' | 'converting' | 'testing' | 'completed' | 'error';
  progress: number;
  errorMessage?: string;
}

export interface ModernizationSession {
  id: string;
  projectName: string;
  totalLines: number;
  chunks: CodeChunk[];
  startTime: number;
}

export enum ConversionStep {
  IDLE = 'IDLE',
  UPLOADING = 'UPLOADING',
  ANALYZING_SYSTEM = 'ANALYZING_SYSTEM',
  CONVERTING_INCREMENTS = 'CONVERTING_INCREMENTS',
  FINALIZING = 'FINALIZING'
}

export interface GitHubUser {
  id: string;
  login: string;
  name: string;
  avatar_url: string;
  role: 'Developer' | 'Admin' | 'Architect';
}

export interface GitHubConfig {
  repo: string;
  branch: string;
  commitMessage: string;
  isPrivate: boolean;
}
