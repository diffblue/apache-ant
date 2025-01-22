package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCUpdateDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCUpdate#setCurrentTime(boolean)}
   *   <li>{@link CCUpdate#setGraphical(boolean)}
   *   <li>{@link CCUpdate#setLog(String)}
   *   <li>{@link CCUpdate#setOverwrite(boolean)}
   *   <li>{@link CCUpdate#setPreserveTime(boolean)}
   *   <li>{@link CCUpdate#setRename(boolean)}
   *   <li>{@link CCUpdate#getCurrentTime()}
   *   <li>{@link CCUpdate#getGraphical()}
   *   <li>{@link CCUpdate#getLog()}
   *   <li>{@link CCUpdate#getOverwrite()}
   *   <li>{@link CCUpdate#getPreserveTime()}
   *   <li>{@link CCUpdate#getRename()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCUpdate ccUpdate = new CCUpdate();

    // Act
    ccUpdate.setCurrentTime(true);
    ccUpdate.setGraphical(true);
    ccUpdate.setLog("Log");
    ccUpdate.setOverwrite(true);
    ccUpdate.setPreserveTime(true);
    ccUpdate.setRename(true);
    boolean actualCurrentTime = ccUpdate.getCurrentTime();
    boolean actualGraphical = ccUpdate.getGraphical();
    String actualLog = ccUpdate.getLog();
    boolean actualOverwrite = ccUpdate.getOverwrite();
    boolean actualPreserveTime = ccUpdate.getPreserveTime();

    // Assert
    assertEquals("Log", actualLog);
    assertTrue(actualCurrentTime);
    assertTrue(actualGraphical);
    assertTrue(actualOverwrite);
    assertTrue(actualPreserveTime);
    assertTrue(ccUpdate.getRename());
  }

  /**
   * Test new {@link CCUpdate} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCUpdate}
   */
  @Test
  public void testNewCCUpdate() {
    // Arrange and Act
    CCUpdate actualCcUpdate = new CCUpdate();

    // Assert
    assertEquals("cleartool", actualCcUpdate.getClearToolCommand());
    assertNull(actualCcUpdate.getDescription());
    assertNull(actualCcUpdate.getTaskName());
    assertNull(actualCcUpdate.getTaskType());
    assertNull(actualCcUpdate.getLog());
    assertNull(actualCcUpdate.getObjSelect());
    assertNull(actualCcUpdate.getViewPath());
    assertNull(actualCcUpdate.getProject());
    assertNull(actualCcUpdate.getOwningTarget());
    assertFalse(actualCcUpdate.getCurrentTime());
    assertFalse(actualCcUpdate.getGraphical());
    assertFalse(actualCcUpdate.getOverwrite());
    assertFalse(actualCcUpdate.getPreserveTime());
    assertFalse(actualCcUpdate.getRename());
    assertTrue(actualCcUpdate.getFailOnErr());
  }
}
