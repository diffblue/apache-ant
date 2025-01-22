package org.apache.tools.ant.helper;

import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class DefaultExecutorDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link DefaultExecutor}
   *   <li>{@link DefaultExecutor#getSubProjectExecutor()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange, Act and Assert
    assertTrue((new DefaultExecutor()).getSubProjectExecutor() instanceof SingleCheckExecutor);
  }
}
