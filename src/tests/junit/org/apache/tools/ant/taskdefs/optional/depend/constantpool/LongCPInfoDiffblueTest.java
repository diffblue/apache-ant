package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import org.junit.Test;

public class LongCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link LongCPInfo}
   *   <li>{@link LongCPInfo#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    LongCPInfo actualLongCPInfo = new LongCPInfo();

    // Assert
    assertEquals("Long Constant Pool Entry: null", actualLongCPInfo.toString());
    assertNull(actualLongCPInfo.getValue());
    assertEquals(2, actualLongCPInfo.getNumEntries());
    assertEquals(5, actualLongCPInfo.getTag());
    assertFalse(actualLongCPInfo.isResolved());
  }

  /**
   * Test {@link LongCPInfo#read(DataInputStream)}.
   * <ul>
   *   <li>Then {@link LongCPInfo} (default constructor) Value longValue is {@code 4708585257725083992}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LongCPInfo#read(DataInputStream)}
   */
  @Test
  public void testRead_thenLongCPInfoValueLongValueIs4708585257725083992() throws IOException {
    // Arrange
    LongCPInfo longCPInfo = new LongCPInfo();

    // Act
    longCPInfo.read(new DataInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));

    // Assert
    assertEquals(4708585257725083992L, ((Long) longCPInfo.getValue()).longValue());
  }
}
