package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class SleepDiffblueTest {
  /**
   * Test new {@link Sleep} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Sleep}
   */
  @Test
  public void testNewSleep() {
    // Arrange and Act
    Sleep actualSleep = new Sleep();

    // Assert
    Location location = actualSleep.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSleep.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSleep.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSleep.getTaskName());
    assertNull(actualSleep.getTaskType());
    assertNull(actualSleep.getProject());
    assertNull(actualSleep.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSleep, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link Sleep#validate()}.
   * <ul>
   *   <li>Given {@link Sleep} (default constructor) Seconds is minus one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Sleep#validate()}
   */
  @Test
  public void testValidate_givenSleepSecondsIsMinusOne_thenThrowBuildException() throws BuildException {
    // Arrange
    Sleep sleep = new Sleep();
    sleep.setSeconds(-1);

    // Act and Assert
    assertThrows(BuildException.class, () -> sleep.validate());
  }
}
