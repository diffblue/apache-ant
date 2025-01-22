package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import org.junit.Test;

public class FloatCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link FloatCPInfo}
   *   <li>{@link FloatCPInfo#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    FloatCPInfo actualFloatCPInfo = new FloatCPInfo();

    // Assert
    assertEquals("Float Constant Pool Entry: null", actualFloatCPInfo.toString());
    assertNull(actualFloatCPInfo.getValue());
    assertEquals(1, actualFloatCPInfo.getNumEntries());
    assertEquals(4, actualFloatCPInfo.getTag());
    assertFalse(actualFloatCPInfo.isResolved());
  }

  /**
   * Test {@link FloatCPInfo#read(DataInputStream)}.
   * <ul>
   *   <li>Then {@link FloatCPInfo} (default constructor) Value floatValue is {@code 13.515953}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FloatCPInfo#read(DataInputStream)}
   */
  @Test
  public void testRead_thenFloatCPInfoValueFloatValueIs13515953() throws IOException {
    // Arrange
    FloatCPInfo floatCPInfo = new FloatCPInfo();

    // Act
    floatCPInfo.read(new DataInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));

    // Assert
    assertEquals(13.515953f, ((Float) floatCPInfo.getValue()).floatValue(), 0.0f);
  }
}
