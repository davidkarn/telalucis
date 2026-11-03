import { Controller, Get, Inject, NotFoundException, Param, Query } from '@nestjs/common';
import { JournalsService } from './journals.service';
import { id_param, paging_params } from './params';

@Controller('journals')
export class JournalsController {
  constructor(@Inject(JournalsService) private readonly journals: JournalsService) {}

  @Get()
  async list() {
    const { items, count } = await this.journals.list();
    return { meta: { count }, items };
  }

  @Get(':journalId')
  async get(@Param('journalId') journalId: string) {
    const journal = await this.journals.get(id_param('journalId', journalId));

    if (!journal) {
      throw new NotFoundException(`No journal with id ${ journalId }`);
    }
    else {
      return journal;
    }
  }

  @Get(':journalId/issues')
  async issues(
    @Param('journalId') journalId: string,
    @Query('offset') offset?: unknown,
    @Query('length') length?: unknown,
  ) {
    const found = await this.journals.issues(id_param('journalId', journalId), paging_params(offset, length));

    if (!found) {
      throw new NotFoundException(`No journal with id ${ journalId }`);
    }
    else {
      return { meta: { count: found.count }, items: found.items };
    }
  }

  @Get(':journalId/issues/:issueId')
  async issue(@Param('journalId') journalId: string, @Param('issueId') issueId: string) {
    const issue = await this.journals.issue(id_param('journalId', journalId), id_param('issueId', issueId));

    if (!issue) {
      throw new NotFoundException(`Journal ${ journalId } has no issue with id ${ issueId }`);
    }
    else {
      return issue;
    }
  }
}
