package org.apache.tools.ant.taskdefs.optional.vss;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class MSVSSCHECKOUTDiffblueTest {
  /**
   * Test new {@link MSVSSCHECKOUT} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MSVSSCHECKOUT}
   */
  @Test
  public void testNewMsvsscheckout() throws BuildException {
    // Arrange and Act
    MSVSSCHECKOUT actualMsvsscheckout = new MSVSSCHECKOUT();

    // Assert
    assertEquals("", actualMsvsscheckout.getFileTimeStamp());
    assertEquals("", actualMsvsscheckout.getGetLocalCopy());
    assertEquals("", actualMsvsscheckout.getLabel());
    assertEquals("", actualMsvsscheckout.getLocalpath());
    assertEquals("", actualMsvsscheckout.getLogin());
    assertEquals("", actualMsvsscheckout.getOutput());
    assertEquals("", actualMsvsscheckout.getQuiet());
    assertEquals("", actualMsvsscheckout.getRecursive());
    assertEquals("", actualMsvsscheckout.getStyle());
    assertEquals("", actualMsvsscheckout.getUser());
    assertEquals("", actualMsvsscheckout.getVersion());
    assertEquals("", actualMsvsscheckout.getVersionDate());
    assertEquals("", actualMsvsscheckout.getVersionDateLabel());
    assertEquals("", actualMsvsscheckout.getVersionLabel());
    assertEquals("", actualMsvsscheckout.getWritable());
    assertEquals("", actualMsvsscheckout.getWritableFiles());
    assertEquals("-C-", actualMsvsscheckout.getComment());
    assertNull(actualMsvsscheckout.getDescription());
    assertNull(actualMsvsscheckout.getTaskName());
    assertNull(actualMsvsscheckout.getTaskType());
    assertNull(actualMsvsscheckout.getVsspath());
    assertNull(actualMsvsscheckout.getProject());
    assertNull(actualMsvsscheckout.getOwningTarget());
    assertEquals(MSVSSConstants.FLAG_AUTORESPONSE_DEF, actualMsvsscheckout.getAutoresponse());
    assertEquals(MSVSSConstants.SS_EXE, actualMsvsscheckout.getSSCommand());
  }
}
