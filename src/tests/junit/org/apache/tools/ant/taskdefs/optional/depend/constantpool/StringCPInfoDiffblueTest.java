package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import org.junit.Test;

public class StringCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link StringCPInfo}
   *   <li>{@link StringCPInfo#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    StringCPInfo actualStringCPInfo = new StringCPInfo();

    // Assert
    assertEquals("String Constant Pool Entry for null[0]", actualStringCPInfo.toString());
    assertNull(actualStringCPInfo.getValue());
    assertEquals(1, actualStringCPInfo.getNumEntries());
    assertEquals(8, actualStringCPInfo.getTag());
    assertFalse(actualStringCPInfo.isResolved());
  }

  /**
   * Test {@link StringCPInfo#read(DataInputStream)}.
   * <ul>
   *   <li>Then {@link StringCPInfo} (default constructor) Value is {@code unresolved}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringCPInfo#read(DataInputStream)}
   */
  @Test
  public void testRead_thenStringCPInfoValueIsUnresolved() throws IOException {
    // Arrange
    StringCPInfo stringCPInfo = new StringCPInfo();

    // Act
    stringCPInfo.read(new DataInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));

    // Assert
    assertEquals("unresolved", stringCPInfo.getValue());
  }
}
