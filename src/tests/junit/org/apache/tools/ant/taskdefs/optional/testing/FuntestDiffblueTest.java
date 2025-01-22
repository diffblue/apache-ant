package org.apache.tools.ant.taskdefs.optional.testing;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.condition.ConditionBase;
import org.junit.Test;

public class FuntestDiffblueTest {
  /**
   * Test {@link Funtest#createCondition()}.
   * <p>
   * Method under test: {@link Funtest#createCondition()}
   */
  @Test
  public void testCreateCondition() {
    // Arrange and Act
    ConditionBase actualCreateConditionResult = (new Funtest()).createCondition();

    // Assert
    assertEquals("component", actualCreateConditionResult.getTaskName());
    Location location = actualCreateConditionResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateConditionResult.getDescription());
    assertNull(actualCreateConditionResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Funtest#setFailOnTeardownErrors(boolean)}
   *   <li>{@link Funtest#setFailureMessage(String)}
   *   <li>{@link Funtest#setFailureProperty(String)}
   *   <li>{@link Funtest#setShutdownTime(long)}
   *   <li>{@link Funtest#setTimeout(long)}
   *   <li>{@link Funtest#getApplicationException()}
   *   <li>{@link Funtest#getTaskException()}
   *   <li>{@link Funtest#getTeardownException()}
   *   <li>{@link Funtest#getTestException()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Funtest funtest = new Funtest();

    // Act
    funtest.setFailOnTeardownErrors(true);
    funtest.setFailureMessage("Failure Message");
    funtest.setFailureProperty("Failure Property");
    funtest.setShutdownTime(1L);
    funtest.setTimeout(10L);
    BuildException actualApplicationException = funtest.getApplicationException();
    BuildException actualTaskException = funtest.getTaskException();
    BuildException actualTeardownException = funtest.getTeardownException();

    // Assert
    assertNull(actualApplicationException);
    assertNull(actualTaskException);
    assertNull(actualTeardownException);
    assertNull(funtest.getTestException());
  }

  /**
   * Test new {@link Funtest} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Funtest}
   */
  @Test
  public void testNewFuntest() {
    // Arrange and Act
    Funtest actualFuntest = new Funtest();

    // Assert
    assertNull(actualFuntest.getDescription());
    assertNull(actualFuntest.getTaskName());
    assertNull(actualFuntest.getTaskType());
    assertNull(actualFuntest.getApplicationException());
    assertNull(actualFuntest.getTaskException());
    assertNull(actualFuntest.getTeardownException());
    assertNull(actualFuntest.getTestException());
    assertNull(actualFuntest.getProject());
    assertNull(actualFuntest.getOwningTarget());
  }
}
