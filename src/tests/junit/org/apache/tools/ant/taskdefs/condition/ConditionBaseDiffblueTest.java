package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import org.apache.tools.ant.taskdefs.ConditionTask;
import org.junit.Test;

public class ConditionBaseDiffblueTest {
  /**
   * Test {@link ConditionBase#countConditions()}.
   * <p>
   * Method under test: {@link ConditionBase#countConditions()}
   */
  @Test
  public void testCountConditions() {
    // Arrange, Act and Assert
    assertEquals(0, (new ConditionTask()).countConditions());
  }

  /**
   * Test {@link ConditionBase#setTaskName(String)}.
   * <p>
   * Method under test: {@link ConditionBase#setTaskName(String)}
   */
  @Test
  public void testSetTaskName() {
    // Arrange
    ConditionTask conditionTask = new ConditionTask();

    // Act
    conditionTask.setTaskName("Name");

    // Assert
    assertEquals("Name", conditionTask.getTaskName());
  }

  /**
   * Test {@link ConditionBase#getTaskName()}.
   * <p>
   * Method under test: {@link ConditionBase#getTaskName()}
   */
  @Test
  public void testGetTaskName() {
    // Arrange, Act and Assert
    assertEquals("condition", (new ConditionTask()).getTaskName());
  }
}
