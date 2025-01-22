package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.junit.Test;

public class BuildEventDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link BuildEvent#BuildEvent(Project)}
   *   <li>{@link BuildEvent#setException(Throwable)}
   *   <li>{@link BuildEvent#getException()}
   *   <li>{@link BuildEvent#getMessage()}
   *   <li>{@link BuildEvent#getPriority()}
   *   <li>{@link BuildEvent#getProject()}
   *   <li>{@link BuildEvent#getTarget()}
   *   <li>{@link BuildEvent#getTask()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Project project = new Project();

    // Act
    BuildEvent actualBuildEvent = new BuildEvent(project);
    Throwable exception = new Throwable();
    actualBuildEvent.setException(exception);
    Throwable actualException = actualBuildEvent.getException();
    String actualMessage = actualBuildEvent.getMessage();
    int actualPriority = actualBuildEvent.getPriority();
    Project actualProject = actualBuildEvent.getProject();
    Target actualTarget = actualBuildEvent.getTarget();

    // Assert
    assertNull(actualMessage);
    assertNull(actualTarget);
    assertNull(actualBuildEvent.getTask());
    assertEquals(3, actualPriority);
    assertSame(exception, actualException);
    assertSame(project, actualBuildEvent.getSource());
    assertSame(project, actualProject);
  }

  /**
   * Test {@link BuildEvent#BuildEvent(Target)}.
   * <p>
   * Method under test: {@link BuildEvent#BuildEvent(Target)}
   */
  @Test
  public void testNewBuildEvent() {
    // Arrange
    Target target = new Target();

    // Act
    BuildEvent actualBuildEvent = new BuildEvent(target);

    // Assert
    assertNull(actualBuildEvent.getMessage());
    assertNull(actualBuildEvent.getException());
    assertNull(actualBuildEvent.getProject());
    assertNull(actualBuildEvent.getTask());
    assertEquals(3, actualBuildEvent.getPriority());
    assertSame(target, actualBuildEvent.getSource());
    assertSame(target, actualBuildEvent.getTarget());
  }

  /**
   * Test {@link BuildEvent#BuildEvent(Task)}.
   * <p>
   * Method under test: {@link BuildEvent#BuildEvent(Task)}
   */
  @Test
  public void testNewBuildEvent2() {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    // Act
    BuildEvent actualBuildEvent = new BuildEvent(task);

    // Assert
    assertNull(actualBuildEvent.getMessage());
    assertNull(actualBuildEvent.getException());
    assertNull(actualBuildEvent.getProject());
    assertNull(actualBuildEvent.getTarget());
    assertEquals(3, actualBuildEvent.getPriority());
    assertSame(task, actualBuildEvent.getSource());
    assertSame(task, actualBuildEvent.getTask());
  }

  /**
   * Test {@link BuildEvent#setMessage(String, int)}.
   * <p>
   * Method under test: {@link BuildEvent#setMessage(String, int)}
   */
  @Test
  public void testSetMessage() {
    // Arrange
    BuildEvent buildEvent = new BuildEvent(new Project());

    // Act
    buildEvent.setMessage("Not all who wander are lost", 1);

    // Assert
    assertEquals("Not all who wander are lost", buildEvent.getMessage());
    assertEquals(1, buildEvent.getPriority());
  }
}
