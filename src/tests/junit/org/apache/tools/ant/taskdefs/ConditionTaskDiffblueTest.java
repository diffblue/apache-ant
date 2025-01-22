package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class ConditionTaskDiffblueTest {
  /**
   * Test new {@link ConditionTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ConditionTask}
   */
  @Test
  public void testNewConditionTask() {
    // Arrange and Act
    ConditionTask actualConditionTask = new ConditionTask();

    // Assert
    assertEquals("condition", actualConditionTask.getTaskName());
    Location location = actualConditionTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualConditionTask.getDescription());
    assertNull(actualConditionTask.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link ConditionTask#execute()}.
   * <ul>
   *   <li>Given {@link ConditionTask} (default constructor) addAvailable {@link Available} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConditionTask#execute()}
   */
  @Test
  public void testExecute_givenConditionTaskAddAvailableAvailable_thenThrowBuildException() throws BuildException {
    // Arrange
    ConditionTask conditionTask = new ConditionTask();
    conditionTask.addAvailable(new Available());

    // Act and Assert
    assertThrows(BuildException.class, () -> conditionTask.execute());
  }

  /**
   * Test {@link ConditionTask#execute()}.
   * <ul>
   *   <li>Given {@link ConditionTask} (default constructor) addAvailable {@link Available} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConditionTask#execute()}
   */
  @Test
  public void testExecute_givenConditionTaskAddAvailableAvailable_thenThrowBuildException2() throws BuildException {
    // Arrange
    ConditionTask conditionTask = new ConditionTask();
    conditionTask.addAvailable(new Available());
    conditionTask.addAvailable(new Available());

    // Act and Assert
    assertThrows(BuildException.class, () -> conditionTask.execute());
  }

  /**
   * Test {@link ConditionTask#execute()}.
   * <ul>
   *   <li>Given {@link ConditionTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConditionTask#execute()}
   */
  @Test
  public void testExecute_givenConditionTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ConditionTask()).execute());
  }
}
