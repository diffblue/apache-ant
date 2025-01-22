package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class BriefJUnitResultFormatterDiffblueTest {
  /**
   * Test new {@link BriefJUnitResultFormatter} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link BriefJUnitResultFormatter}
   */
  @Test
  public void testNewBriefJUnitResultFormatter() {
    // Arrange, Act and Assert
    assertEquals("Null Test: ", (new BriefJUnitResultFormatter()).formatTest(null));
  }
}
