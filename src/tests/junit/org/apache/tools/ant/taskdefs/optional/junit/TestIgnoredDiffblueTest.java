package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertSame;
import org.junit.Test;

public class TestIgnoredDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TestIgnored#TestIgnored(junit.framework.Test)}
   *   <li>{@link TestIgnored#getTest()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Class<Object> testClass = Object.class;
    JUnit4TestMethodAdapter test = new JUnit4TestMethodAdapter(testClass, new String[]{"Method Names"});

    // Act and Assert
    assertSame(test, (new TestIgnored(test)).getTest());
  }
}
