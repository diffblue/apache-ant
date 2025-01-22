package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CvsDiffblueTest {
  /**
   * Test new {@link Cvs} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Cvs}
   */
  @Test
  public void testNewCvs() {
    // Arrange and Act
    Cvs actualCvs = new Cvs();

    // Assert
    assertTrue(actualCvs.getErrorStream() instanceof LogOutputStream);
    assertTrue(actualCvs.getOutputStream() instanceof LogOutputStream);
    assertTrue(actualCvs.getExecuteStreamHandler() instanceof PumpStreamHandler);
    assertNull(actualCvs.getDest());
    assertNull(actualCvs.getPassFile());
    assertNull(actualCvs.getDescription());
    assertNull(actualCvs.getTaskName());
    assertNull(actualCvs.getTaskType());
    assertNull(actualCvs.getCommand());
    assertNull(actualCvs.getCvsRoot());
    assertNull(actualCvs.getCvsRsh());
    assertNull(actualCvs.getPackage());
    assertNull(actualCvs.getTag());
    assertNull(actualCvs.getProject());
    assertNull(actualCvs.getOwningTarget());
    assertEquals(0, actualCvs.getPort());
    assertTrue(actualCvs.getModules().isEmpty());
  }
}
