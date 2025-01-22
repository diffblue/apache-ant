package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.WaitFor.Unit;
import org.junit.Test;

public class WaitForDiffblueTest {
  /**
   * Test {@link WaitFor#WaitFor()}.
   * <p>
   * Method under test: {@link WaitFor#WaitFor()}
   */
  @Test
  public void testNewWaitFor() {
    // Arrange and Act
    WaitFor actualWaitFor = new WaitFor();

    // Assert
    assertEquals("waitfor", actualWaitFor.getTaskName());
    Location location = actualWaitFor.getLocation();
    assertNull(location.getFileName());
    assertNull(actualWaitFor.getDescription());
    assertNull(actualWaitFor.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link WaitFor#WaitFor(String)}.
   * <p>
   * Method under test: {@link WaitFor#WaitFor(String)}
   */
  @Test
  public void testNewWaitFor2() {
    // Arrange and Act
    WaitFor actualWaitFor = new WaitFor("Task Name");

    // Assert
    assertEquals("Task Name", actualWaitFor.getTaskName());
    Location location = actualWaitFor.getLocation();
    assertNull(location.getFileName());
    assertNull(actualWaitFor.getDescription());
    assertNull(actualWaitFor.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link WaitFor#execute()}.
   * <ul>
   *   <li>Given {@link WaitFor#WaitFor()} addAvailable {@link Available} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WaitFor#execute()}
   */
  @Test
  public void testExecute_givenWaitForAddAvailableAvailable_thenThrowBuildException() throws BuildException {
    // Arrange
    WaitFor waitFor = new WaitFor();
    waitFor.addAvailable(new Available());
    waitFor.addAvailable(new Available());

    // Act and Assert
    assertThrows(BuildException.class, () -> waitFor.execute());
  }

  /**
   * Test {@link WaitFor#execute()}.
   * <ul>
   *   <li>Given {@link WaitFor#WaitFor()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WaitFor#execute()}
   */
  @Test
  public void testExecute_givenWaitFor_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new WaitFor()).execute());
  }

  /**
   * Test {@link WaitFor#calculateCheckEveryMillis()}.
   * <p>
   * Method under test: {@link WaitFor#calculateCheckEveryMillis()}
   */
  @Test
  public void testCalculateCheckEveryMillis() {
    // Arrange, Act and Assert
    assertEquals(WaitFor.DEFAULT_CHECK_MILLIS, (new WaitFor()).calculateCheckEveryMillis());
  }

  /**
   * Test {@link WaitFor#calculateMaxWaitMillis()}.
   * <p>
   * Method under test: {@link WaitFor#calculateMaxWaitMillis()}
   */
  @Test
  public void testCalculateMaxWaitMillis() {
    // Arrange, Act and Assert
    assertEquals(WaitFor.DEFAULT_MAX_WAIT_MILLIS, (new WaitFor()).calculateMaxWaitMillis());
  }

  /**
   * Test Unit {@link Unit#getValues()}.
   * <p>
   * Method under test: {@link Unit#getValues()}
   */
  @Test
  public void testUnitGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{Unit.MILLISECOND, Unit.SECOND, Unit.MINUTE, Unit.HOUR, Unit.DAY, Unit.WEEK},
        (new Unit()).getValues());
  }

  /**
   * Test Unit new {@link Unit} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Unit}
   */
  @Test
  public void testUnitNewUnit() {
    // Arrange and Act
    Unit actualUnit = new Unit();

    // Assert
    assertNull(actualUnit.getValue());
    assertEquals(-1, actualUnit.getIndex());
    assertArrayEquals(new String[]{Unit.MILLISECOND, Unit.SECOND, Unit.MINUTE, Unit.HOUR, Unit.DAY, Unit.WEEK},
        actualUnit.getValues());
  }
}
