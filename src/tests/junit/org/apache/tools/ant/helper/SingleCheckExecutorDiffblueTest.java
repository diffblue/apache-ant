package org.apache.tools.ant.helper;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Executor;
import org.junit.Test;

public class SingleCheckExecutorDiffblueTest {
  /**
   * Test {@link SingleCheckExecutor#getSubProjectExecutor()}.
   * <p>
   * Method under test: {@link SingleCheckExecutor#getSubProjectExecutor()}
   */
  @Test
  public void testGetSubProjectExecutor() {
    // Arrange
    SingleCheckExecutor singleCheckExecutor = new SingleCheckExecutor();

    // Act
    Executor actualSubProjectExecutor = singleCheckExecutor.getSubProjectExecutor();

    // Assert
    assertTrue(actualSubProjectExecutor instanceof SingleCheckExecutor);
    assertSame(singleCheckExecutor, actualSubProjectExecutor);
  }
}
